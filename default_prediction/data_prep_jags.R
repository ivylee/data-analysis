#####################################
# Title: Clean up data for JAGS/Stan
# Author: Ivy Lee
# Date: 11 May 2013
#####################################

train <- read.csv("cs-training.csv")
train$X <- NULL
names(train) <- c("class", "reutil", "age", "past30.59", "debt", "income",
"credits", "past90", "realest", "past60.89", "deps")
test <- read.csv("cs-test.csv")
test$X <- NULL
names(test) <- c("class", "reutil", "age", "past30.59", "debt", "income",
"credits", "past90", "realest", "past60.89", "deps")

clean <- function(data) {
  cols <- c(4, 8, 10)
  for (col in cols) {
    na_mask <- data[,col]==98|data[,col]==96
    data[,col][na_mask] <- 0
  }
  has_past <- rep(1, nrow(data))
  has_past[na_mask] <- 0
  has_age <- rep(1, nrow(data))
  has_age[data[,3]==0] <- 0
  has_income <- rep(1, nrow(data))
  has_income[is.na(data[,6])] <- 0
  data[,6][is.na(data[,6])] <- 0
  has_dep <- rep(1, nrow(data))
  has_dep[is.na(data[,11])] <- 0
  data[,11][is.na(data[,11])] <- 0
  data <- cbind(data, has_age, has_past, has_income, has_dep) 
  return(data)
}

train_data <- clean(train)

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

