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

   # pick an historical path
   z <- returns_qtr |>
      filter(year(date)>1972) |>
      select(date,cash_ret,bond_ret,stock_ret,cpi_growth) |>
      slice_head(n=pds+1) |>
      # set first period to zero as base
      mutate(across(2:5,\(x) x = ifelse(date == min(date),0,x)))

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

   # pick an historical path
   z <- returns_qtr |>
      filter(year(date)>1972) |>
      select(date,cash_ret,bond_ret,stock_ret,cpi_growth) |>
      slice_head(n=pds+1) |>
      # set first period to zero as base
      mutate(across(2:5,\(x) x = ifelse(date == min(date),0,x)))

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

bind_cols(z[,1],zt) |>
   ggplot(aes(date,stock_balance)) +
   geom_line() +
   geom_line(aes(date,bm_balance),linetype=2) +
   geom_hline(yintercept = 100) +
   annotate("text",family = "serif",label = "Starting Value: $100mm",
            x=as.yearqtr("1975 Q1","%Y Q%q"), y= 102) +
   labs(title= "How Patient Are You?",
        y = "Endowment Balance ($mm)",
        x= "Date",
        caption="Source: Equity Returns - Robert Shiller; Simulation - Art Steinmetz") +
   theme_light(base_family = "serif",base_size = 16) +
   annotate("text",x=as.yearqtr("1973 Q1","%Y Q%q"),y=120, family = "serif",size = 5, hjust = 0,
            label = "$100mm Starting Endowment Value. Assume Fixed Annual Spend,
            Starting at $5mm, Growing With Inflation.") +
   annotate("text",x=3100,y=65, family = "serif",size = 5,
            label = "100% U.S. Equities") +
   annotate("text",x=3100,y=95, family = "serif",size = 5,
            label = "70/30 Benchmark") +
   scale_x_yearquarter(date_breaks = "year")
