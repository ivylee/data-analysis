library(e1071)
library(biglm)

# read data
source("data_cleaning_eda.R")

# randomly choose around 80% data for training, 20% for testing
get_data <- function(data) {
# return training data frame and test data frame
  data[ ,1] <- NULL
  n <- nrow(data)
  set.seed(123)
  data <- data[sample(n,n), ]
  mask <- sample(n, round(n*0.8))
  train <- data[mask, ]
  test <- data[-mask, ]
  return(list(train=train, test=test))
}

logitfit <- function(train, test) {
#return fit object and prediction
    f <-SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+
    age+
    NumberOfTime30.59DaysPastDueNotWorse+
    DebtRatio+
    MonthlyIncome+
    NumberOfOpenCreditLinesAndLoans+
    NumberOfTimes90DaysLate+
    NumberRealEstateLoansOrLines+
    NumberOfTime60.89DaysPastDueNotWorse+
    NumberOfDependents
    fit <- bigglm(f, train, family=binomial(link="logit"),
    chunksize=5000)
    pred <- predict(fit, test, type="response")
    return(list(fit=fit, pred=pred))
}

naivebayesfit <- function(train, test) {
#return fit object and prediction
  fit <- naiveBayes(SeriousDlqin2yrs ~ ., train)
  pred <- predict(fit, test, type="raw")[ ,2]
  return(list(fit=fit, pred=pred))
}

error <- function(pred, test, cutoff) {
# return error rate
  label <- as.vector(test$SeriousDlqin2yrs)
  errors <- vector()
  for (i in 1:length(cutoff)) {
    pred[pred >= cutoff[i]] <- 1
    pred[pred < cutoff[i]] <- 0
    diff <- pred - label 
    error <- 1 - length(diff[diff==0])/length(label)
    errors <- rbind(c(cutoff[i], error), errors)
  }
  return(errors)
}

dfs <- get_data(data)
train <- dfs$train
test <- dfs$test
lgfit <- logitfit(train, test)
nbfit <- naivebayesfit(train, test)
cutoff <- seq(0.4, 0.6, by=0.05)
lgerr <- error(lgfit$pred, test, cutoff)
nberr <- error(nbfit$pred, test, cutoff)

# submission
train2 <- data
test2 <- read.csv("cs-test.csv")
test2 <- sub_na(test2)
test2[ ,2] <- 0
lgfit2 <- logitfit(train2, test2)
result1 <- data.frame(test2$X, lgfit2$pred)
write.table(result1, file="submit11.csv", sep=",", row.names=F, col.names=F)
nbfit2 <- naivebayesfit(train2, test2)
result2 <- data.frame(test2$X, nbfit2$pred)
write.table(result2, file="submit2.csv", sep=",", row.names=F, col.names=F)
