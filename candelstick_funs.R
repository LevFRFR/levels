if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(tidyverse)

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
  levels <- list()
  
  # from the third index to the 3 to last index...
  for (i in 3:(nrow(df)-2)) {
    # check if the index is fractal support
    if (isSupport(df, i)) {
      # if yes, append to the levels as support
      df.Low <- df %>% 
        select(., ends_with("Low")) %>% 
        pull(.)
      
      to_append <- c(i, df.Low[i])
      
      levels <- list.append(levels, to_append)
    }
    # check if the index is fractal resistance
    else if (isResistance(df, i)) {
      # if yes, append to the levels as resistance
      df.High <- df %>% 
        select(., ends_with("High")) %>% 
        pull(.)
      
      to_append <- c(i, df.High[i])
      
      levels <- list.append(levels, to_append)
    }
  }
  
  return(levels)
}


# function for # adding levels to the chart
plotLevels <- function(fig, dataset, levels) {
  
  # initiate a line shape object
  line <- list(
    type = "line",
    line = list(width= 4, color = 'rgb(205, 12, 24)'),
    xref = "x",
    yref = "y"
  )
  
  lines <- list()
  
  for (i in levels) {
    
    # date start and end for the line
    line[["x0"]] <- dataset$Date[i[1]]-1
    line[["x1"]] <- dataset$Date[i[1]]+1
      #max(dataset$Date)
    
    # price level for the line
    line[c("y0", "y1")] <- i[2]
    
    lines <- c(lines, list(line))
  }
  
  fig2 <- fig %>% layout(shapes = lines) 
  
  return(fig2)
}

# test comment
