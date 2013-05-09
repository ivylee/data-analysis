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

standardize <- function(data) {
   data_std <- scale(data[ ,-c(1,12:15)], center=TRUE, scale=TRUE)
   data_std <- cbind(data[ ,c(1,12:15)], data_std)
   return(data_std)
}

split <- function(data) {
  set.seed(123)
  rows <- nrow(data)
  mask <- sample(1:rows, 0.7*rows)
  train <- data[mask,]
  test <- data[-mask,]
  return(list(train=train, test=test))
}

train_data <- standardize(clean(train))
train_split <- split(train_data)
train_set <- train_split$train
test_set <- train_split$test
train_set_x <- train_set[ ,-1]
train_set_y <- train_set[ ,1]
test_set_x <- test_set[ ,-1]
test_set_y <- test_set[ ,1]
test_data <- standardize(clean(test))
test_data_x <- test_data[ ,-1]
