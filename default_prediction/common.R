#########################################
# Title: Libraries and shared functions
# Author: Ivy Lee
# Date: 12 May 2013
#########################################

library(Matrix)
library(glmnet)
library(ROCR)
library(e1071)
library(parallel)
library(doMC)
library(foreach)

registerDoMC(8)

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

subsample<- function(data, size, pos_rate) {
  pos_mask <- train_data$class==1
  neg_mask <- train_data$class==0
  train_data_pos <- train_data[pos_mask,]
  train_data_neg <- train_data[neg_mask,]
  n_pos <- size*pos_rate
  n_neg <- size*(1-pos_rate)
  sub_data_pos <- train_data_pos[sample(1:nrow(train_data_pos), n_pos),]
  sub_data_neg <- train_data_neg[sample(1:nrow(train_data_neg), n_neg),]
  sub_sample <- rbind(sub_data_pos, sub_data_neg)
  sub_sample <- sub_sample[sample.int(nrow(sub_sample)),]
  return(sub_sample)
}
