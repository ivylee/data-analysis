source("data_prep.R")
source("common.R")

# L1 Regularized
l1logit <- function(train_set, test_set) {
  x <- as.matrix(train_set[,-1])
  y <- Matrix(train_set[,1], sparse=TRUE)
  cv <- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=5)
  pred <- predict(cv, test_set[,-1], s="lambda.min", type="response")
  p <- prediction(pred, test_set[,1])
  return(list(p=p, cv=cv))
}

# Naive Bayes
nb <- function(train_set, test_set) {
  train <- naiveBayes(train_set[ ,-1], train_set[ ,1])
  fitted <- predict(train, test_set[ ,-1], type="raw")[,2]
  p <- prediction(fitted, test_set[ ,1])
  return(p=p)
}

### Bagging
#
## Naive Bayes
#bagging_nb <- function(train_set, test_set, iterations) {
#  n <- nrow(train_set)
#  probs <- foreach(i=1:iterations, .combine=cbind) %dopar% {
#    sampled <- sample(1:n, n, replace=TRUE)
#    x <- train_set[sampled, -1]
#    y <- train_set[sampled, 1]
#    nb <- naiveBayes(x, y) #5.299
#    fitted <- predict(nb, test_set[ ,-1], type="raw") #32.125
#    fitted[,2]
#  }
#  pred <- rowMeans(probs)
#  p <- prediction(pred, test_set[, 1])
#  return(p)
#}
#
## Logistic
#bagging_logit <- function(train_set, test_set, iterations) {
#  n <- nrow(train_set)
#  probs <- foreach(i=1:iterations, .combine=cbind) %dopar% {
#    sampled <- sample(1:n, n, replace=TRUE)
#    x <- as.matrix(train_set[sampled, -1])
#    y <- Matrix(train_set[sampled, 1], sparse=TRUE)
#    cv <- cv.glmnet(x, y, family="binomial", type.measure="auc",
#    nfolds=5) # 104.439
#    fitted <- predict(cv, test_set[ ,-1], s="lambda.min", type="response")
#    fitted
#  }
#  pred <- rowMeans(probs)
#  p <- prediction(pred, test_set[, 1])
#  return(p)
#}

nb.p <- nb(train_set, test_set)
nb.roc <- get_perf(nb.p)
l1logit <- l1logit(train_set, test_set)
l1logit.cv <- l1logit$cv
l1logit.p <- l1logit$p
l1logit.roc <- get_perf(l1logit.p)
pdf("l1logit_cv_class.pdf")
plot(l1logit.cv)
dev.off()
#bag1000.nb.p <- bagging_nb(train_set, test_set, 1000)
#bag1000.nb.roc <- get_perf(bag1000.nb.p)
#bag1000.l1logit.p <- bagging_logit(train_set, test_set, 1000)
#bag1000.l1logit.roc <- get_perf(bag1000.l1logit.p)

# Ensemble
nb.l1logit.prob <- rowMeans(cbind(nb.p@predictions[[1]], l1logit.p@predictions[[1]]))
nb.l1logit.p <- prediction(nb.l1logit.prob, test_set[, 1])
nb.l1logit.roc <- get_perf(nb.l1logit.p)
pdf("nb_l1logit_avg.pdf")
plot(l1logit.roc, main="Performance On Hold Out Data Set")
lines(nb.roc@x.values[[1]], nb.roc@y.values[[1]], col="red")
lines(nb.l1logit.roc@x.values[[1]], nb.l1logit.roc@y.values[[1]], col="green")
legend(0.2,0.3, c("L1 Regularized Logistic Regression", "Naive Bayes", "Ensemble"), lty=c(1,1,1), col=c("black", "red", "green"))
dev.off()

# Plots
#pdf("complots.pdf")
#plot(l1logit.roc, main="Performance On Hold Out Data Set")
#lines(nb.roc@x.values[[1]], nb.roc@y.values[[1]], col="red")
#lines(bag1000.l1logit.roc@x.values[[1]],
#bag1000.l1logit.roc@y.values[[1]], col="blue")
#lines(bag1000.nb.roc@x.values[[1]], bag1000.nb.roc@y.values[[1]],
#col="green")
#legend(0.2,0.3, c("L1 Regularized Logistic Regression", "Naive Bayes",
#"L1 Regularizaed Logistic Regression with Bagging", "Naive Bayes with
#Bagging"), lty=c(1,1), col=c("black", "red", "blue", "green"))
#dev.off()

# Sub sample
#train_sub_set <- train_set[sample(1:nrow(train_set), 0.1*nrow(train_set)), ]
#test_sub_set <- test_set[sample(1:nrow(test_set), 0.1*nrow(test_set)), ]
#sub.nb.p <- nb(train_sub_set, test_sub_set)$p
#sub.nb.roc <- get_perf(sub.nb.p)
#sub.l1logit.p <- l1logit(train_sub_set, test_sub_set)$p
#sub.l1logit.roc <- get_perf(sub.l1logit.p)
#sub.bag1000.nb.p <- bagging_nb(train_sub_set, test_sub_set, 1000)
#sub.bag1000.nb.roc <- get_perf(sub.bag1000.nb.p)
#sub.bag1000.l1logit.p <- bagging_logit(train_sub_set, test_sub_set, 1000)
#sub.bag1000.l1logit.roc <- get_perf(sub.bag1000.l1logit.p)
#pdf("sub_complots.pdf")
#plot(sub.l1logit.roc, main="Performance On 10% Hold Out Data Set")
#lines(sub.nb.roc@x.values[[1]], sub.nb.roc@y.values[[1]], col="red")
#lines(sub.bag1000.l1logit.roc@x.values[[1]],
#sub.bag1000.l1logit.roc@y.values[[1]], col="blue")
#lines(sub.bag1000.nb.roc@x.values[[1]], sub.bag1000.nb.roc@y.values[[1]],
#col="green")
#legend(0.2,0.3, c("L1 Regularized Logistic Regression", "Naive Bayes",
#"L1 Regularizaed Logistic Regression with Bagging", "Naive Bayes with
#Bagging"), lty=c(1,1), col=c("black", "red", "blue", "green"))
#dev.off()
