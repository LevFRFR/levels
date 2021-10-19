# Lev Gorokhov
# UCID 30043014
# September 8th, 2020

## Please make sure dplyr is loaded prior to quantmod to avoid functions being masked.

## FQ = Final Question Scenario (3a-e)

## Approach:
# Create automated process that retrieves desired stocks' information
# Manipulate each stocks data to gain data points on the number of shares, net value, daily returns
# Daily return can be calculated using the formula: (todays price - yesterdays price)/yesterdays price,
# applied onto the net value variable, by stock, or by portfolio


# PACKAGES ----------------------------------------------------------------
library("tidyverse")
library("lubridate")
library("Hmisc")
library("quantmod")
library("gdata")
library("ggplot2")


# IMPORT DATA WITH QUANTMOD -----------------------------------------------
# Choose Tickers
list_of_tickers <- Cs(MSFT, AAPL, TSLA, AMZN) # AMZN, MSFT, AAPL, CRM, ORCL, NFLX, QCOM

# Choose source
ticker_source <- "yahoo"

# Assign portfolios
names(list_of_tickers) <- c(1, 1, 1, 1) # 1, 2, 2, 3, 3, 3, 3

# Set the investment limit per portfolio
investment_per_portfolio <- as.numeric(25000)

# pull the ticker data for the desired time period
start_date <- "2018-02-28" # "2005-01-10"

end_date <- "2021-03-01" # "2020-09-02"

# the following code applies bulk changes to the data
dfList <- lapply(
  X = list_of_tickers, # function applies onto the list of tickers
  FUN = function(stocks) as.data.frame( # transform into a dataframe
    getSymbols(
      stocks, 
      src = ticker_source, 
      from = ymd(start_date), 
      to = ymd(end_date), 
      auto.assign = FALSE
    )
  ) %>% rownames_to_column(var = "date") # create the column of dates from row names
  %>% mutate(
    date = ymd(date), # data type to date
    daily_return = (.[[5]] - lag(x = .[[5]], n = 1))/lag(x = .[[5]], n = 1), # take 5th column of closing prices and create daily return using lag
    daily_return_FQ = ( # mean between low and high prices for the last question
      rowMeans(select(., matches(".Low|.High"))) - lag(x = rowMeans(select(., matches(".Low|.High"))), n = 1)
    ) / lag(x = rowMeans(select(., matches(".Low|.High"))), n = 1)
  )
)

# Assign names to stock datasets accordingly
stock_list <- setNames(dfList, list_of_tickers)

# import into environment
list2env(stock_list, envir = .GlobalEnv)

# clean up
keep(list = c("stock_list", list_of_tickers, "ticker_source", "list_of_tickers", "investment_per_portfolio"), sure = T)


# DEFINE FUNCTIONS --------------------------------------------------------
# Provides ROI, given a stock in the form of a dataframe
get_roi <- function(stock, use_closing_prices = T) {
  
  if (use_closing_prices == T) {
    roi <- (stock[stock$date == max(stock$date), 5] - stock[stock$date == min(stock$date), 5]) / stock[stock$date == min(stock$date), 5]
  } else {
    roi <- (mean(as.numeric(stock[stock$date == max(stock$date), 3:4])) - mean(as.numeric(stock[stock$date == min(stock$date), 3:4]))) / mean(as.numeric(stock[stock$date == min(stock$date), 3:4]))
  }
  
  return(roi)
}

# Provides Purchase cost, given a stock in the form of a dataframe
get_purchase_cost <- function(stock, use_closing_prices = T) {
  
  if (use_closing_prices == T) {
    purchase_cost <- stock[stock$date == min(stock$date), 5]
  } else {
    purchase_cost <- mean(as.numeric(stock[stock$date == min(stock$date), 3:4]))
  }
  
  return(purchase_cost)
}

# Provides Current price, given a stock in the form of a dataframe
get_current_price <- function(stock, use_closing_prices = T) {
  
  if (use_closing_prices == T) {
    current_price <- stock[stock$date == max(stock$date), 5]
  } else {
    current_price <- mean(as.numeric(stock[stock$date == max(stock$date), 3:4]))
  }
  
  return(current_price)
}

# Creates a portfolio dataframe consisting of all the included stocks' closing prices, net values, and daily returns
portfolio_as.data.frame <- function(portfolio_number, use_closing_prices = T) {
  
  if (use_closing_prices == F) {
    portfolio_overview = FQ_portfolio_overview
    column = c(3:4) # lo & hi
  } else {
    column = c(5, 5)
  }
  
  stocks_in_portfolio <- paste(list_of_tickers[names(list_of_tickers) == portfolio_number], sep = ", ")
  
  portfolio <- stock_list[stocks_in_portfolio]
  
  portfolio_breakdown <- data.frame(date = portfolio[[1]][1])
  
  for (stock in stocks_in_portfolio) {
    # find net value of the stock = shares * current closing price
    net_value <- data.frame(net_value = rowMeans(portfolio[[stock]][,c(column)], na.rm=TRUE) * portfolio_overview$number_of_shares[portfolio_overview$symbol == stock])
    
    closing_price <- data.frame(closing_price = rowMeans(portfolio[[stock]][,c(column)], na.rm=TRUE))
    
    var.name <- paste0(stock, ".net_value")
    names(net_value) <- var.name
    
    var.name <- paste0(stock, ".closing_price")
    names(closing_price) <- var.name
    
    # Solve for individual daily returns using net value of each stock
    daily_return <- net_value %>% transmute(
      daily_return = (.[[1]] - lag(x = .[[1]], n = 1))/lag(x = .[[1]], n = 1),
    )
    
    var.name <- paste0(stock, ".daily_return")
    names(daily_return) <- var.name
    
    portfolio_breakdown <- cbind(portfolio_breakdown, closing_price, net_value, daily_return)
  }
  
  portfolio_net_value <- portfolio_breakdown %>%  
    mutate(
      net_price = rowSums(select(., ends_with(".closing_price"))),
      net_value = rowSums(select(., ends_with(".net_value"))),
      daily_return = (net_value - lag(x = net_value, n = 1))/lag(x = net_value, n = 1)
    )
  
  return(portfolio_net_value)
}

# Creates a graph for a portfolio. Similar operations as in 'portfolio_as.data.frame'
graph_portfolio <- function(portfolio_number, use_closing_prices = T) {
  
  if (use_closing_prices == F) {
    portfolio_overview = FQ_portfolio_overview
    column = c(3:4) # lo & hi
  } else {
    column = c(5, 5) # mean of 2 identical columns is the identical column
  }
  
  stocks_in_portfolio <- paste(list_of_tickers[names(list_of_tickers) == portfolio_number], sep = ", ")
  
  portfolio <- stock_list[stocks_in_portfolio]
  
  portfolio_breakdown <- data.frame(date = portfolio[[1]][1])
  
  for (stock in stocks_in_portfolio) {
    # find net value of the stock = shares * current closing price
    net_value <- data.frame(net_value = rowMeans(portfolio[[stock]][,c(column)], na.rm=TRUE) * portfolio_overview$number_of_shares[portfolio_overview$symbol == stock])
    
    closing_price <- data.frame(closing_price = rowMeans(portfolio[[stock]][,c(column)], na.rm=TRUE))
    
    var.name <- paste0(stock, ".net_value")
    names(net_value) <- var.name
    
    var.name <- paste0(stock, ".closing_price")
    names(closing_price) <- var.name
    
    # Solve for individual daily returns using net value of each stock
    daily_return <- net_value %>% transmute(
      daily_return = (.[[1]] - lag(x = .[[1]], n = 1))/lag(x = .[[1]], n = 1),
    )
    
    var.name <- paste0(stock, ".daily_return")
    names(daily_return) <- var.name
    
    portfolio_breakdown <- cbind(portfolio_breakdown, closing_price, net_value, daily_return)
  }
  
  portfolio_net_value <- portfolio_breakdown %>%  
    mutate(
      net_price = rowSums(select(., ends_with(".closing_price"))),
      net_value = rowSums(select(., ends_with(".net_value"))),
      daily_return = (net_value - lag(x = net_value, n = 1))/lag(x = net_value, n = 1)
    )
  
  ylab <- round(seq(min(portfolio_net_value$net_value), ceiling(max(portfolio_net_value$net_value)), length.out = 5), digits = -1)
  
  plot <- ggplot(
    portfolio_net_value, 
    aes(x = date, y = net_value)
  ) +
    geom_line() +
    expand_limits(
      x = c(min(portfolio_net_value$date), max(portfolio_net_value$date)),
      y = c(0, ceiling(max(portfolio_net_value$net_value) + 1/10*max(portfolio_net_value$net_value)))
    ) +
    scale_y_continuous(
      labels = paste0("USD ", ylab / 1000, " K"),
      breaks = ylab
    ) + geom_hline(yintercept = max(portfolio_net_value$net_value), color = "red") + 
    theme_bw() +
    ggtitle(
      label = "Portfolio Net Value",
      subtitle = paste("Stocks included:", paste0(stocks_in_portfolio, collapse = ", "))
    ) + 
    labs(
      caption = paste0("(based on data from ", ticker_source, ")"),
      x = "Year",
      y = "Portfolio Value"
    ) + 
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return(plot)
}

# DEFINE PORTFOLIOS -------------------------------------------------------
# Initialize df for all stocks
all_stocks_df <- data.frame(
  portfolio = character(),
  symbol = character(), 
  roi = double(), 
  purchase_cost = double(), 
  current_price = double(),
  mean_daily_return = double(),
  sd_daily_return = double()
)

# Fill it in with data
for (i in 1:length(stock_list)) {
  
  # Portfolio number
  all_stocks_df[i, 1] <- names(list_of_tickers)[i]
  
  # Stock symbol
  all_stocks_df[i, 2] <- str_extract(string = names(stock_list[[i]])[2], pattern = ".*(?=\\.)")
  
  # ROI per Stock
  all_stocks_df[i, 3] <- get_roi(stock_list[[i]])
  
  # Purchase cost of the stock
  all_stocks_df[i, 4] <- get_purchase_cost(stock_list[[i]])
  
  # Current Price of the stock
  all_stocks_df[i, 5] <- get_current_price(stock_list[[i]])
  
  # Mean Daily Return
  all_stocks_df[i, 6] <- mean(stock_list[[i]]$daily_return, na.rm = T)
  
  # Standard deviation of daily returns
  all_stocks_df[i, 7] <- sd(stock_list[[i]]$daily_return, na.rm = T)
  
}

# Turn it into a portfolio overview with dynamic number of shares measure
portfolio_overview <- all_stocks_df %>% 
  group_by(portfolio) %>% 
  add_tally(name = "multiplication_factor") %>% 
  mutate(
    number_of_shares = investment_per_portfolio / multiplication_factor / purchase_cost
  ) %>%
  select(-multiplication_factor) %>% 
  ungroup()

# Portfolio 1
portfolio_1 <- portfolio_as.data.frame(portfolio_number = 1)
View(portfolio_1)

# Portfolio 2
portfolio_2 <- portfolio_as.data.frame(portfolio_number = 2)
View(portfolio_2)

# Portfolio #
portfolio_3 <- portfolio_as.data.frame(portfolio_number = 3)
View(portfolio_3)


# QUESTIONS ---------------------------------------------------------------
## 1a. --------------------------------------------------------------------
# (10 points) Of the seven companies, which one has the stock with the highest long-run ROI?
#   long-run roi = (value on sep 1, 2020 - value on jan 10, 2005)/value on jan 10, 2005
portfolio_overview %>% arrange(desc(roi))

# NFLX has the highest long-run ROI of 345%


## 1b. --------------------------------------------------------------------
# (10 points) Which one has the highest mean daily return?
portfolio_overview %>% arrange(desc(mean_daily_return))

## NFLX, on average, has the highest mean daily return of 0.20% 


## 1c. -------------------------------------------------------------------- 
# (10 points) Which one has the lowest standard deviation of its daily return (i.e., lowest risk)?
portfolio_overview %>% arrange(sd_daily_return)

## MSFT, on average, has the lowest risk. The standard deviation of its daily return is 1.72%


## 2a. -------------------------------------------------------------------- 
# (10 points) Of the three portfolios, which one has the highest long-run ROI?
`highest portfolio` <- portfolio_overview %>% 
  group_by(portfolio) %>% 
  transmute(ROI = mean(roi)) %>% 
  distinct() %>% arrange(desc(ROI))
View(`highest portfolio`)

# Portfolio number 3 has the highest long-run ROI of 103.78%


## 2b. -------------------------------------------------------------------- 
# (10 points) Plot the value of the three portfolios over time, either as a combined plot or as separate plots.
#   What was the highest value achieved by any of the three portfolios between January 10, 2005 and September1, 2020? 
#   Which portfolio achieved this value?

# Portfolio 1
graph_portfolio(portfolio_number = 1) # USD 418160 

# Portfolio 2
graph_portfolio(portfolio_number = 2) # USD 293610

# Portfolio #
graph_portfolio(portfolio_number = 3) # USD 523920

# Portfolio # 3 has the highest achieved value of ~ USD 524 thousand


## 2c. -------------------------------------------------------------------- 
# (10 points) How correlated is Portfolio 1’s daily return with Portfolio 2’s daily return? How correlated is Portfolio 1’s daily return with Portfolio 3’s daily return?
#   How correlated is Portfolio 2’s daily return with Portfolio 3’s daily return?
#   Which pair of portfolios has the highest correlation of their daily returns? 
#   Plot three separate scatter plots for the pairwise daily returns, and then find the correlation coefficients using the cor() function

# Correlation between Portfolio 1 and 2
p1.p2_cor <- cor(x = na.omit(portfolio_1$daily_return), y = na.omit(portfolio_2$daily_return))

p1.p2_cor # 0.4723567

qplot(
  x = na.omit(portfolio_1$daily_return),
  y = na.omit(portfolio_2$daily_return),
  geom = c("point"),
  size=I(1),
  alpha=I(0.3)
) +
  geom_abline(slope = p1.p2_cor) +
  labs(
    title = "Correlation of Daily Returns between Portfolio 1 & 2",
    x = "Portfolio 1: Daily Returns",
    y = "Portfolio 2: Daily Returns"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Correlation between Portfolio 1 and 3
p1.p3_cor <- cor(x = na.omit(portfolio_1$daily_return), y = na.omit(portfolio_3$daily_return))

p1.p3_cor # 0.5036399

qplot(
  x = na.omit(portfolio_1$daily_return),
  y = na.omit(portfolio_3$daily_return),
  geom = c("point"),
  size=I(1),
  alpha=I(0.3)
) +
  geom_abline(slope = p1.p3_cor) +
  labs(
    title = "Correlation of Daily Returns between Portfolio 1 & 3",
    x = "Portfolio 1: Daily Returns",
    y = "Portfolio 3: Daily Returns"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Correlation between Portfolio 2 and 3
p2.p3_cor <- cor(x = na.omit(portfolio_2$daily_return), y = na.omit(portfolio_3$daily_return))

p2.p3_cor # 0.463358

qplot(
  x = na.omit(portfolio_2$daily_return),
  y = na.omit(portfolio_3$daily_return),
  geom = c("point"),
  size=I(1),
  alpha=I(0.3)
) +
  geom_abline(slope = p2.p3_cor) +
  labs(
    title = "Correlation of Daily Returns between Portfolio 2 & 3",
    x = "Portfolio 2: Daily Returns",
    y = "Portfolio 3: Daily Returns"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# While all portfolios are positively correlated,
# portfolio 1 & 3 have the highest correlation of daily returns - approximately 50.36%


## 3a-e. ------------------------------------------------------------------ 
# 3a-e. (10 points) One question you might wonder is how robust your responses are to slight changes in the stock prices. 
# Suppose that instead of the Closing price for each day, you had used the average between the High and Low prices for that day for your daily return and long-run ROI calculations. 
# Of the previous questions, which of your responses would change?
all_stocks_df_FQ <- data.frame(
  portfolio = character(),
  symbol = character(), 
  roi = double(), 
  purchase_cost = double(), 
  current_price = double(),
  mean_daily_return = double(),
  sd_daily_return = double()
)

for (i in 1:length(stock_list)) {
  
  # Portfolio number
  all_stocks_df_FQ[i, 1] <- names(list_of_tickers)[i]
  
  # Stock symbol
  all_stocks_df_FQ[i, 2] <- str_extract(string = names(stock_list[[i]])[2], pattern = ".*(?=\\.)")
  
  # ROI per Stock
  all_stocks_df_FQ[i, 3] <- get_roi(stock_list[[i]], use_closing_prices = F)
  
  # Purchase cost of the stock
  all_stocks_df_FQ[i, 4] <- get_purchase_cost(stock_list[[i]], use_closing_prices = F)
  
  # Current Price of the stock
  all_stocks_df_FQ[i, 5] <- get_current_price(stock_list[[i]], use_closing_prices = F)
  
  # Mean Daily Return
  all_stocks_df_FQ[i, 6] <- mean(stock_list[[i]]$daily_return_FQ , na.rm = T)
  
  # Standard deviation of daily returns
  all_stocks_df_FQ[i, 7] <- sd(stock_list[[i]]$daily_return_FQ , na.rm = T)
  
}

FQ_portfolio_overview <- all_stocks_df_FQ %>% 
  group_by(portfolio) %>% 
  add_tally(name = "multiplication_factor") %>% 
  mutate(
    number_of_shares = investment_per_portfolio / multiplication_factor / purchase_cost
  ) %>%
  select(-multiplication_factor) %>% 
  ungroup()

# Portfolio 1
FQ_portfolio_1 <- portfolio_as.data.frame(portfolio_number = 1, use_closing_prices = F)

# Portfolio 2
FQ_portfolio_2 <- portfolio_as.data.frame(portfolio_number = 2, use_closing_prices = F)

# Portfolio #
FQ_portfolio_3 <- portfolio_as.data.frame(portfolio_number = 3, use_closing_prices = F)


# 3a ----------------------------------------------------------------------
# NFLX still has the highest long-run ROI of 333%


# 3b ----------------------------------------------------------------------
## NFLX, on average, has the highest daily return of 3.3%


# 3c ----------------------------------------------------------------------
## MSFT, on average, has the lowest risk. The standard deviation of its daily return is 1.37%


# 3d-e --------------------------------------------------------------------
`fq highest portfolio long-run ROI` <- FQ_portfolio_overview %>% 
  group_by(portfolio) %>% 
  transmute(ROI = mean(roi)) %>% 
  distinct() %>% arrange(desc(ROI))
View(`fq highest portfolio long-run ROI`)

# Through using the mean of Low and High of the stock price during the day, the calculated ROI has slightly changed:
all.equal(`fq highest portfolio long-run ROI`, `highest portfolio`) # Mean relative difference: 2.33%

# The top ROI now is: 100.29%


# By using mean of low and high prices, we can see slight mean relative differences across all components of the portfolios.
all.equal(FQ_portfolio_1, portfolio_1)

all.equal(FQ_portfolio_2, portfolio_2)

all.equal(FQ_portfolio_3, portfolio_3)


# The correlation of daily returns between portfolios changed slightly, but do not warrant the change in the final answer:

# Correlation between Portfolio 1 and 2
p1.p2_cor_fq <- cor(x = na.omit(FQ_portfolio_1$daily_return), y = na.omit(FQ_portfolio_2$daily_return))

p1.p2_cor_fq # 0.4144895
all.equal(p1.p2_cor_fq, p1.p2_cor) # "Mean relative difference: 13.96%"

# Correlation between Portfolio 1 and 3
p1.p3_cor_fq <- cor(x = na.omit(FQ_portfolio_1$daily_return), y = na.omit(FQ_portfolio_3$daily_return))

p1.p3_cor_fq # 0.4590386
all.equal(p1.p3_cor_fq, p1.p3_cor) # "Mean relative difference: 9.72%"

# Correlation between Portfolio 2 and 3
p2.p3_cor_fq <- cor(x = na.omit(FQ_portfolio_2$daily_return), y = na.omit(FQ_portfolio_3$daily_return))

p2.p3_cor_fq # 0.3951845
all.equal(p2.p3_cor_fq, p2.p3_cor) # "Mean relative difference: 17.25%"

# None of the answers besides ROI and individual stock daily returns has changed