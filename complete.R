complete <- function(directory, id=1:332) {
  compl <- data.frame("id" = numeric(),    
                      'nobs' =  numeric())
  for (i in id) {
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", i), ".csv", sep = "")
    monitor_data <- read.csv(path)
    j<- which(id==i)
    list_j <- c(i, sum(complete.cases(monitor_data)))
    compl[j,] <- list_j
  }
  compl
}