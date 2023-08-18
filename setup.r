# load libraries -----------------------------------
library(MASS)
library(Rcpp)
library(tidyverse)
library(gt)
library(gghighlight)
library(ggpattern)
library(tidyquant)
library(lubridate)
library(tsibble)
library(slider)
library(skimr)
library(PerformanceAnalytics)

# load returns ------------------------------------
#load shiller long term returns
load(file="data/shiller.rdata")
returns <- shiller |>
   mutate(date = yearmonth(date))
# data wrangling ---------------------------------------------
# convert monthly to quarterly returns
# Create US passive BM going wayyy back
returns_qtr <- returns |>
   mutate(bm_ret = 0.7*stock_ret + 0.3*bond_ret,) |>
   # 12-month change, monthly
   mutate(cpi_growth = (CPI/lag(CPI,12))^(1/12)-1) |>
   mutate(qtr = yearquarter(date)) |>
   select(qtr,date,cash_ret,stock_ret,bond_ret,bm_ret,cpi_growth) |>
   group_by(qtr) |>
   summarise(across(where(is.numeric),~ prod(1+.x)-1)) |>
   rename(date = qtr) |>
   mutate(cpi_annl = (1+cpi_growth)^4-1)
# -------------------r functions ---------------------------
tidy_to_xts <- function(df){
   xt <- df |>
      mutate(date = ceiling_date(as.Date(date),unit="month")) |>
      column_to_rownames(var="date") |>
      as.xts()
   return(xt)
}


rolling_ret <- function(returns, years = 10,pds_per_year = 4) {
   returns |>
      mutate(across(where(is.numeric),
                    function(x)
                       (slide_prod(1 + x,
                                   before = years * pds_per_year - 1,
                                   complete = TRUE) ^ (1 / 10)
                       ) - 1)) |>
      na.omit()
}



get_return_struct <- function(returns){
   obs <-as.matrix(na.omit(select(returns,cash_ret,bond_ret,stock_ret,cpi_growth)))
   means <- colMeans(obs)
   std_dev <- apply(obs,2,sd)
   cov_mat <- cov(obs)
   cor_mat <- cor(obs)
   return(list(means = means,std_dev = std_dev,cov_mat = cov_mat,cor_mat = cor_mat))
}
ret_struct <- get_return_struct(returns_qtr)


get_rolling_cum <-  function(returns,
                             years = 10,
                             pds_per_year = 4,
                             base_value = 1) {
   returns |>
      mutate(across(where(is.numeric),
                    function(x)
                       (slide_prod(1 + x,
                                   before = years * pds_per_year - 1,
                                   complete = TRUE)
                        )
                    )
             ) |>
      na.omit() |>
      mutate(across(where(is.numeric),\(x) base_value*x))
}

# compute some stats comparing combinations of assets
rel_perf <- function(returns,horizon=10,pds_per_year = 4){

   roll_perf_all <- function(returns,
                             horizon = 10,
                             pds_per_year = 4) {
      rel_ret_series <- returns |>
         mutate(across(where(is.numeric),
                       function(x)
                          (slide_prod(
                             1 + x,
                             before = horizon * pds_per_year - 1,
                             complete = TRUE
                          ) ^ (1 / horizon)
                          ) - 1)) |>
         na.omit()
      return(rel_ret_series)
   }

   rel_ret_2series <- function(rp, series1, series2) {
      rp <- rp |>
         transmute(date, outperformance = {{series1}} - {{series2}},{{series1}},{{series2}}) |>
         na.omit()

      rp <- rename_with(rp, ~ paste0(names(rp)[3],"_vs_",names(rp)[4],recycle0 = TRUE),
                        contains("outperformance"))
      rp_table <- rp |>
         rename(outperformance = 2) |>
         mutate(series1_better = outperformance > 0) |>
         summarise(
            count = n(),
            outperform_count = sum(series1_better),
            average = round(mean(outperformance) * 100, 2)
         ) |>
         mutate(series1_better_pct = round(outperform_count / count * 100),
                .before = "average") |>
         mutate(horizon = horizon, .before = everything()) |>
         mutate(rel_per = nest(rename(rp,outperformance = 2))) |>
         unnest(cols = rel_per) |>
         select(horizon, count, series1_better_pct, average, data) |>
         mutate(comparison = names(rp[2]),.before="horizon")
      return(rp_table)
   }

   roll_perf <- roll_perf_all(returns_qtr,horizon,pds_per_year=4)

   rel_ret <- bind_rows(rel_ret_2series(roll_perf,stock_ret,cash_ret),
                        rel_ret_2series(roll_perf,stock_ret,bm_ret),
                        rel_ret_2series(roll_perf,stock_ret,bond_ret),
                        rel_ret_2series(roll_perf,bond_ret,cash_ret),
                        rel_ret_2series(roll_perf,bm_ret,cash_ret))
   return(rel_ret)
}

cppFunction('
NumericVector get_balance(NumericVector returns,
                   NumericVector cash_flow,
                   int num_rows,
                   bool constant_draw = false,
                   double annl_draw = -0.04) {
    NumericVector balance(num_rows);
    balance[0] = cash_flow[0];
    for (int n = 1; n < num_rows; n++) {
        if (!constant_draw && (n % 4 == 3)) {
            cash_flow[n] = balance[n-1] * annl_draw;
        }
        balance[n] = ((balance[n-1] * (1 + returns[n])) + cash_flow[n]);
        if (balance[n] < 0) {
            balance[n] = 0;
        }
    }
    return balance;
}
')

CB_R <- function(returns,cash_flow,num_rows,constant_draw = FALSE, annl_draw=-.04){
   balance <- numeric(num_rows)
   balance[1] <- cash_flow[1]
   for (n in 2:num_rows){
      if (!constant_draw & (n %% 4 ==0))  {
         cash_flow[n] <- balance[n-1] * annl_draw
         }
      balance[n] <- ((balance[n-1] * (1+returns[n])) + cash_flow[n])
            if (balance[n] < 0) {balance[n] <- 0}
   }
   return(balance)
}

# generate one monte carlo path
# generate one Monte Carlo terminal value
get_path <- function(run=1,
                     ret_struct,
                     final_value_only = TRUE,
                     constant_draw = TRUE,
                     annl_draw = -.04,
                     years=10,
                     base_value=100,
                     use_empirical = FALSE
                     ){
   # TEST CONSTANTS
   # constant_draw = FALSE
   # use_empirical = FALSE
   #years=10
   # base_value = 100
   #
   # run is a dummy variable - no effect
   # create multivariate distribution
   pds <- years * 4
   # cash in and cash out

   # constant,but adjsut for CPI later
   cash_flow <- c(100,rep(c(0,0,annl_draw*100,0),years))
   # no withdrawals
   # cash_flow <- c(100,rep(0,pds))
   # # OPTION 1 use a multivariate normal distribution
   # # distribution will have population structure but the sample won't necessarily
   # z <- mvrnorm(pds,
   #              mu=ret_struct$means,
   #              Sigma=ret_struct$cov_mat,
   #              empirical=use_empirical) |>
   #      as_tibble()
   # test
   # z[,] = 0

   # OPTION 2 use historical samples
   z <- returns_qtr |> select(cash_ret,bond_ret,stock_ret,cpi_growth) |>
     na.omit() |>
     sample_n(pds,replace = FALSE)

   # prepend initial values
   z <- rbind(0,z)

   zt <- cbind(z,cash_flow)|>
      as_tibble() |>
      # make 70/30 benchmark
      mutate(bm_ret = stock_ret*.7 + bond_ret * .3,.before = "cpi_growth") |>
      # adjust for inflation
      mutate(cpi_index = cumprod(1 + cpi_growth)) |>
      mutate(cash_flow = cash_flow * ifelse(constant_draw,cpi_index,1)) |>
      mutate(cum_cash_flow = cumsum(cash_flow)-base_value) |>
      # build quarterly account balances
      mutate(across(cash_ret:bm_ret,\(x) get_balance(x,cash_flow,pds+1,constant_draw,annl_draw))) |>
      #mutate(across(cash_ret:bm_ret,\(x) CB_R(x,cash_flow,pds+1,constant_draw,annl_draw))) |>
      rename_with(.fn= \(x) str_replace(x,"ret","balance")) |>
      select(contains("balance"))
   if (final_value_only) return(tail(zt,1))
   else {
      zt <- rowid_to_column(zt,var="period") |>
      cbind(run) |>
      select(run,period,everything())
      return(zt)
   }
}

# cash in and out
do_MC <- function(runs=1,
                  years = 10,
                  final_value_only= TRUE,
                  base_value = 100,
                  use_empirical = FALSE,
                  constant_draw = TRUE,
                  annl_draw = -.04) {
   #assumes quarterly samples
   terminal_values <- 1:runs |>
      set_names() |>
      # DO the runs. This is the slow bit
      # map(get_terminal_values, ret_struct)|>
      map(get_path, ret_struct,
          final_value_only = final_value_only,
          years=years,
          base_value = base_value,
          use_empirical = use_empirical,
          constant_draw = constant_draw,
          annl_draw = annl_draw) |>
      bind_rows() |>
      as_tibble()
   return(terminal_values)
}

mc_hist <- function(mc) {
   mc_fmt <- paste0("Mean Annl. Return: ",format(mc$mean_annl_ret,digits=3),"%",
                    "\n",
                    "Std.Dev.: ",format(mc$sd__annl_ret,digits=3),"% ",
                    "\n",
                    "Avg.Annl.Vol.: ",format(mc$mean_annl_ret,digits=3),"% ")

   mc$data[[1]] |> filter(Quarter == mc$years * 4 + 1) |>
      ggplot(aes(value)) + geom_histogram(binwidth = 20) +
      labs(
         title = glue::glue("{mc$sample} Monte Carlo"),
         subtitle = glue::glue("Sampling {mc$sample_size} Periods With Replacement"),
         x = glue::glue("Value of $100 After {mc$years} Years"),
         y = glue::glue("Count out of {mc$runs} Trials")
      ) +
      # scale_x_continuous(breaks = seq(0, 400, 20)) +
      geom_vline(
         xintercept = 0,
         color = "red",
         size = 1,
         linetype = 2
      ) +
      # annotate("text", 90, mc$runs / 5, label = "Rf = 2.0%", color = "red") +
      annotate(
         "text",
         80,
         mc$runs / 10,
         label = paste0(mc$cash_wins_pct, "% of Runs\nWorse than Rf"),
         color = "red"
      ) +
      annotate("text", 250, mc$runs / 5, label = mc_fmt, color = "black")
}
mc_path_plot <- function(mc){
   mc$data[[1]] |>
      ggplot(aes(Quarter,value,color=run)) + geom_line() +
      gghighlight(tail(value,n=1) < 100,use_direct_label = FALSE) +
      theme(legend.position = "none") +
      # geom_line(data=dummy_tbill_path,mapping=aes(Quarter,rf),color="red",linewidth=1,linetype=2) +
      labs(
         title=glue::glue("{mc$sample} Monte Carlo Paths"),
         #subtitle = "Highlighted Paths Underperform 2% risk-free rate",
         y=glue::glue("Value of $100 After {mc$years} Years"))
}
