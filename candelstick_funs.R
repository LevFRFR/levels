library(dplyr)
library(plotly)
library(quantmod)
library(lubridate)

# fractal functions

# it is identified by 5 candles

# support fractal function
# 3rd candle has the lowests low price

# the previous two candles have decreasing lows

# and the next two candles have increasing lows

# thus the 3rd candle's lowest low point becomes the support

isSupport <- function(
  dataset, # dataset on which to find the support
  i # ???
) {
  
  df.Low <- dataset %>% 
    select(., ends_with("Low")) %>% 
    pull(.)
  
  support <- df.Low[i] <= df.Low[i-1] & df.Low[i] <= df.Low[i+1] & df.Low[i+1] <= df.Low[i+2] & df.Low[i-1] <= df.Low[i-2]
 
  return(support) 
}

# resistance fractal function
isResistance <- function(
  dataset, # dataset on which to find the resistance
  i # ???
) {
  
  df.High <- dataset %>% 
    select(., ends_with("High")) %>% 
    pull(.)
  
  resistance <- df.High[i] >= df.High[i-1] & df.High[i] >= df.High[i+1] & df.High[i+1] >= df.High[i+2] & df.High[i-1] >= df.High[i-2]
  
  return(resistance) 
}

# function for finding levels in the dataset
findLevels <- function(df) {
  # accumulator for levels
  levels <- data.frame(price = NULL)
  
  # from the third index to the 3 to last index...
  for (i in 3:(nrow(df)-2)) {
    # check if the index is fractal support
    if (isSupport(df, i)) {
      # if yes, append to the levels as support
      df.Low <- df %>% 
        select(., ends_with("Low")) %>% 
        pull(.)
      
      levels[i,"price"] <- df.Low[i]
      
    }
    # check if the index is fractal resistance
    else if (isResistance(df, i)) {
      # if yes, append to the levels as resistance
      df.High <- df %>% 
        select(., ends_with("High")) %>% 
        pull(.)
      
      levels[i,"price"] <- df.High[i]
    }
  }
  
  return(na.omit(levels))
}

# function for finding levels in the dataset (Cleaner)
findLevelsCleaner <- function(df) {
  levels = data.frame(price = NULL)
  
  volatility = mean(
    x = pull(select(df, ends_with("High")) - select(df, ends_with("Low"))),
    na.rm = T
  )
  
  isFarFromLevel <- function(l) {
    
    accum <- numeric(0)
    
    for (x in 1:nrow(levels)) {
      accum <- c(accum, abs(l - levels[x, "price"]) < volatility)
    }
    
    return(sum(accum, na.rm = T) == 0)
  }
  
  for (i in 3:(nrow(df)-2)) {
    # check if the index is fractal support
    if (isSupport(df, i)) {
      # if yes, append to the levels as support
      df.Low <- df %>%
        select(., ends_with("Low")) %>%
        pull(.)
      
      l <- df.Low[i]
      
      if (isFarFromLevel(l)) {
        levels[i, "price"] <- l
      }
    }
    # check if the index is fractal resistance
    else if (isResistance(df, i)) {
      # if yes, append to the levels as resistance
      df.High <- df %>%
        select(., ends_with("High")) %>%
        pull(.)
      
      l <- df.High[i]
      
      if (isFarFromLevel(l)) {
        levels[i, "price"] <- l
      }
    }
  }
  
  return(na.omit(levels))
}

# function for # adding levels to the chart
plotLevels <- function(fig, dataset, levels) {
  
  levels$index <- as.numeric(rownames(levels))
  
  # initiate a line shape object
  line <- list(
    type = "line",
    line = list(width= 4, color = 'rgb(205, 12, 24)'),
    xref = "x",
    yref = "y"
  )
  
  lines <- list()
  
  for (i in 1:nrow(levels)) {
    
    # date start and end for the line
    line[["x0"]] <- dataset$Date[levels[i, "index"]]-1
    line[["x1"]] <- max(dataset$Date) #dataset$Date[i[1]]+1
      #
    
    # price level for the line
    line[c("y0", "y1")] <- levels[i, "price"]
    
    lines <- c(lines, list(line))
  }
  
  fig2 <- fig %>% layout(shapes = lines) 
  
  return(fig2)
}

# function that brings it all together
showLevels <- function(stock, last_n_days) {
  
  stock_name <- getSymbols(
    stock,
    src='yahoo'
  )
  
  # basic example of ohlc charts
  dataset <- data.frame(Date=index(get(stock_name)), coredata(get(stock_name))) %>% 
    tail(last_n_days)
  
  fig <- dataset %>% plot_ly(
    x = ~Date, type="candlestick",
    open = ~.[[2]], 
    close = ~.[[5]],
    high = ~.[[3]], 
    low = ~.[[4]]
  )  %>% 
    layout(
      title = paste0(last_n_days, " Day Chart"),
      xaxis = list(rangebreaks = list(list(bounds=list("sat", "mon"))))
    ) %>% 
    plotLevels(dataset, findLevelsCleaner(dataset))
  
  return(fig)
  
}

