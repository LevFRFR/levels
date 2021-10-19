if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(plotly,
       quantmod,
       rlist,
       lubridate)
source("candelstick_funs.R")

# start_date <- "2020-03-15" # "2005-01-10"

# end_date <- "2020-07-15" # "2020-09-02"

getSymbols(
  "TSLA",src='yahoo'#,
  #from = ymd(start_date), to = ymd(end_date)
)

# basic example of ohlc charts
dataset <- data.frame(Date=index(TSLA),coredata(TSLA)) %>% 
  tail(120)

fig <- dataset %>% plot_ly(
  x = ~Date, type="candlestick",
  open = ~TSLA.Open, close = ~TSLA.Close,
  high = ~TSLA.High, low = ~TSLA.Low
)  %>% 
  layout(
    # title = "30 Day Chart"#,
    # xaxis = list(rangebreaks = list(list(bounds=list("sat", "mon"))))
  )

fig

dataset_levels <- findLevels(dataset)

fig2 <- fig %>% plotLevels(dataset, dataset_levels)

fig2


# # isFarFromLevel ----------------------------------------------------------
# 
# df = dataset
# 
# # rough estimation of volatility
# volatility = mean(
#   x = pull(select(df, ends_with("High")) - select(df, ends_with("Low"))), 
#   na.rm = T
# )
# 
# levels <- list()
# 
# # from the third index to the 3 to last index...
# for (i in 3:(nrow(df)-2)) {
#   # check if the index is fractal support
#   if (isSupport(df, i)) {
#     # if yes, append to the levels as support
#     df.Low <- df %>% 
#       select(., ends_with("Low")) %>% 
#       pull(.)
#     
#     l <- df.Low[i]
#     if (isFarFromLevel(df, level = l)) {
#       levels <- list.append(levels, c(i, l))
#     }
#   }
#   # check if the index is fractal resistance
#   else if (isResistance(df, i)) {
#     # if yes, append to the levels as resistance
#     df.High <- df %>% 
#       select(., ends_with("High")) %>% 
#       pull(.)
#     
#     l <- df.High[i]
#     if (isFarFromLevel(df, level = l)) {
#       levels <- list.append(levels, c(i, l))
#     }
#   }
# }
# 
