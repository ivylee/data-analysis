library(ggplot2)

train <- read.csv("cs-training.csv")
train$X <- NULL
test <- read.csv("cs-test.csv")
test$X <- NULL

set_na <- function(data) {
  cols <- c(4, 8, 10)
  for (col in cols) {
    na_mask <- data[,col]==98|data[,col]==96
    data[,col][na_mask] <- NA
  }
  data[,3][data[,3]==0] <- NA
  return(data)
}

train <- set_na(train)
test <- set_na(test)
train_pos <- train[train$SeriousDlqin2yrs==1,]
train_neg <- train[train$SeriousDlqin2yrs==0,]

binning <- function(data, col) {
  if (col==1) {
    data[,col] <- as.factor(data[,col])
  }
  else if (col==4|col==8|col==10) {
    data[,col][is.na(data[,col])] <- -1
    data[,col] <- cut(data[,col], 
    breaks=c(c(-1:7),max(data[,col],na.rm=T)+1),right=F)
  }
  else if (col==9) {
    data[,col] <- cut(data[,col], 
    breaks=c(c(0:15),max(data[,col])+1),right=F)
  }
  else if (col==11) {
    data[,col][is.na(data[,col])] <- -1
    data[,col] <- cut(data[,col], 
    breaks=c(c(-1:8),max(data[,col])+1),right=F)
  }
  else {
    q0 <- quantile(data[,col], 0, na.rm=T)
    q1 <- quantile(data[,col], 1/8, na.rm=T)
    q2 <- quantile(data[,col], 2/8, na.rm=T)
    q3 <- quantile(data[,col], 3/8, na.rm=T)
    q4 <- quantile(data[,col], 4/8, na.rm=T)
    q5 <- quantile(data[,col], 5/8, na.rm=T)
    q6 <- quantile(data[,col], 6/8, na.rm=T)
    q7 <- quantile(data[,col], 7/8, na.rm=T)
    q8 <- quantile(data[,col], 1, na.rm=T)
    if (col==3|col==6) {
      data[,col][is.na(data[,col])] <- -1
      data[,col] <- cut(data[,col], 
      breaks=c(-1,q0,q1,q2,q3,q4,q5,q6,q7,q8+1),right=F)
    }
    else {
      data[,col] <- cut(data[,col], breaks=c(q0,q1,q2,q3,q4,q5,q6,q7,q8),
      include.lowest=T)
    }
  }
  return(data[,col])
}

dis_train <- train
dis_test <- test
for (i in 1:11) {
  dis_train[,i] <- binning(train, i)
  dis_test[,i] <- binning(test, i)
}

