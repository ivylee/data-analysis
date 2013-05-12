######################################### 
# Title: Libraries and shared functions
# Author: Ivy Lee
# Date: 11 May 2013
#########################################

library(Matrix)
library(glmnet)
library(ROCR)
library(e1071)
library(parallel)
library(doMC)
library(foreach)

registerDoMC(2)

get_perf <- function(prediction, type="roc") {
  if (type=="fr") {
    perf <- performance(prediction, "fpr", "fnr")
  }
  else {
    perf <- performance(prediction, "tpr", "fpr")
  }
  return(perf)
}

submit <- function(pred, filename) {
  p <- data.frame(pred=pred)
  write.csv(p, file=filename) 
}
