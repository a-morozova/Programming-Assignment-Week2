pollutantmean <- function(directory, pollutant, id=1:332) {
  filePaths <- list.files((paste(getwd(), "/", directory, "/", sep = "")), "\\.csv$", full.names = TRUE)
  result <- do.call(rbind, lapply(filePaths, read.csv))
  sub_id <- NULL
  for (i in id) {sub_id <- rbind(result[result$ID == i,], sub_id)}
  mean(sub_id[[pollutant]], na.rm=TRUE)
  }