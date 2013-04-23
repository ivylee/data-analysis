# clean mem
rm(list=ls())

data <- read.csv("cs-training.csv")

# null/na obs 
get_missing <- function(data) {
  missing <- vector()
  for (i in 1:obs) {
    for (j in 1:var) {
      if (is.na(data[i,j]) | is.null(data[i,j])) {
        missing <- rbind(missing, data[i, ])
      }
    }
  }
  return(missing)
}

# histogram and summary stats
data_summary <- function(data) {
  print(summary(data))
  cols <- ncol(data)
  pdf("hist.pdf")
  par(mfrow=c(4,3))
  for (i in 2:cols) {
    hist(data[ ,i], xlab=names(data)[i], main="")
  }
  dev.off()
}

# substitute NA by median
sub_na <- function(data) {
  data$NumberOfDependents[is.na(data$NumberOfDependents)]<-median(data$NumberOfDependents,
  na.rm=T)
  data$MonthlyIncome[is.na(data$MonthlyIncome)] <- median(data$MonthlyIncome, na.rm=T)
  return(data)
}

data <- sub_na(data)
