corr <- function(directory, threshold=0) {
  store <-numeric()
  for (i in 1:332) {
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", i), ".csv", sep = "")
    monitor_data <- read.csv(path)
    if ((sum(complete.cases(monitor_data)))>threshold) 
    {store[(length(store) + 1)] <- (cor(monitor_data$nitrate, monitor_data$sulfate, use = "na.or.complete"))}
  } 
  store
}