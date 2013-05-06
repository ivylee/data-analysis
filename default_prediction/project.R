library(Matrix)
library(glmnet)
library(ROCR)
library(e1071)

train <- read.csv("cs-training.csv")
train$X <- NULL
names(train) <- c("class", "reutil", "age", "past30.59", "debt", "income",
"credits", "past90", "realest", "past60.89", "deps")
test <- read.csv("cs-test.csv")
test$X <- NULL

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

train_cl <- clean(train)
test_cl <- clean(test)
test_x <- as.matrix(test_cl[,-1])

# Perfect separation
#f1 <-
#class~reutil+age+has_age+past30.59+has_past+debt+income+has_income+credits+past90+realest+past60.89+deps+has_dep
#fit1 <- glm(f1, family=binomial(link=logit), data=train_cl)

# L1 Regularized
x <- as.matrix(train_cl[,-1])
y <- Matrix(train_cl[,1], sparse=TRUE)
f2cv <- cv.glmnet(x, y, family="binomial", type.measure="auc")
f2cv$lambda.min # 0.01600967
coef(f2cv)
#                       1
#(Intercept) -2.153392672
#reutil       .
#age         -0.004179375
#past30.59    0.419407589
#debt         .
#income       .
#credits      .
#past90       0.639612271
#realest      .
#past60.89    0.521797851
#deps         .
#has_age      .
#has_past    -0.622161958
#has_income   .
#has_dep      .
f2fitted <- predict(f2cv, x, s="lambda.min", type="response")
f2p <- prediction(f2fitted, as.vector(y))
f2perf <- performance(f2p,"tpr","fpr")
f2auc <- performance(f2p, "auc", fpr.stop=0.5)
f2pred <- predict(f2cv, test_x, s="lambda.min", type="response") 
f2submit <- data.frame(pred=f2pred)
write.csv(f2submit, file="l1logit.csv") # 0.821799

# Naive Bayes
nb <- naiveBayes(x, as.vector(y))
nbfitted <- predict(nb, x, type="raw")
nbp <- prediction(nbfitted[,2], as.vector(y))
nbperf <- performance(nbp,"tpr","fpr")
nbauc <- performance(nbp, "auc", fpr.stop=0.5)
nbpred <- predict(nb, test_x, type="raw")
nbsubmit <- data.frame(pred=nbpred[,2])
write.csv(nbsubmit, file="nb.csv") # 0.629919

