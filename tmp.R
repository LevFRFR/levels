l = dataset_levels[[1]][2]

isFarFromLevel <- function(l) {
  
  logicals <- numeric(0)
  
  for (x in dataset_levels) {
    
    logicals <- c(logicals, abs(l-x[2]) < volatility)
    
  }
  
  return(sum(logicals, na.rm = T) == 0)
  
}

for (level in dataset_levels) {
  print(level)
  # print(isFarFromLevel(level[2]))
}
