get_rolling_rel_performance <-
   function(returns,
            years = 10,
            title = "Rolling 10-Year Return",
            pds_per_year = 4) {
      returns |>
         mutate(across(where(is.numeric),
                       function(x)
                          (slide_prod(1 + x, before = years * pds_per_year - 1, complete = TRUE) ^ (1/10)) - 1)
         ) |>
         mutate(stock_advantage = stock_ret - bond_ret) |>
         mutate(Advantage = ifelse(stock_advantage > 0,"Stocks Better","Bonds Better")) |>
         na.omit()
   }


pad <- tibble(date = yearquarter(seq(as.yearqtr("1802 Q1"), as.yearqtr("1870 Q4"), by=1/4)))

relperf <- returns_qtr |>
   select(date,stock_ret,bond_ret) |>
   get_rolling_rel_performance(years = 10) |>
   full_join(pad) |>
   arrange(date) |>
   #change all numeric NA to zero
   mutate(across(where(is.numeric), ~replace_na(.,0))) |>
   mutate(Advantage = replace_na(Advantage,"Stocks Better"))

relperf |>
   ggplot(aes(date, stock_advantage, fill = Advantage)) + geom_col() +
   geom_hline(yintercept = 0) +
   scale_color_tq() +
   # show negative values in red
   scale_fill_manual(values = c("orangered1", "skyblue3")) +
   scale_x_yearquarter(breaks = "10 years", date_labels = "%Y") +
   scale_y_continuous(labels = scales::percent) +
   # increase size of x labels
   theme_tq() +
   theme(axis.text.x = element_text(size = 10)) +
   labs(title = "Stocks minus Bonds over 10-Year Holding Periods",
        x = "Date", y = "Annualized Return Difference")



[1] "aliceblue"       "blue"            "blue1"           "blue2"           "blue3"           "blue4"           "blueviolet"      "cadetblue"
[9] "cadetblue1"      "cadetblue2"      "cadetblue3"      "cadetblue4"      "cornflowerblue"  "darkblue"        "darkslateblue"   "deepskyblue"
[17] "deepskyblue1"    "deepskyblue2"    "deepskyblue3"    "deepskyblue4"    "dodgerblue"      "dodgerblue1"     "dodgerblue2"     "dodgerblue3"
[25] "dodgerblue4"     "lightblue"       "lightblue1"      "lightblue2"      "lightblue3"      "lightblue4"      "lightskyblue"    "lightskyblue1"
[33] "lightskyblue2"   "lightskyblue3"   "lightskyblue4"   "lightslateblue"  "lightsteelblue"  "lightsteelblue1" "lightsteelblue2" "lightsteelblue3"
[41] "lightsteelblue4" "mediumblue"      "mediumslateblue" "midnightblue"    "navyblue"        "powderblue"      "royalblue"       "royalblue1"
[49] "royalblue2"      "royalblue3"      "royalblue4"      "skyblue"         "skyblue1"        "skyblue2"        "skyblue3"        "skyblue4"
[57] "slateblue"       "slateblue1"      "slateblue2"      "slateblue3"      "slateblue4"      "steelblue"       "steelblue1"      "steelblue2"
[65] "steelblue3"      "steelblue4"
[1] "darkred"         "indianred"       "indianred1"      "indianred2"      "indianred3"      "indianred4"      "mediumvioletred" "orangered"
[9] "orangered1"      "orangered2"      "orangered3"      "orangered4"      "palevioletred"   "palevioletred1"  "palevioletred2"  "palevioletred3"
[17] "palevioletred4"  "red"             "red1"            "red2"            "red3"            "red4"            "violetred"       "violetred1"
[25] "violetred2"      "violetred3"      "violetred4"
>
