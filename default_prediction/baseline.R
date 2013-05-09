# Perfect separation
#f1 <-
#class~reutil+age+has_age+past30.59+has_past+debt+income+has_income+credits+past90+realest+past60.89+deps+has_dep
#fit1 <- glm(f1, family=binomial(link=logit), data=train_set)

# L1 Regularized
l1logit <- function(train_set, test_set) {
  x <- as.matrix(train_set[,-1])
  y <- Matrix(train_set[,1], sparse=TRUE)
  cv <- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=5)
  pred <- predict(cv, as.matrix(test_set[,-1]), s="lambda.min", type="response")
  p <- prediction(pred, test_set[,1])
  return(list(pred=pred, p=p, cv=cv))
}
#l1logit.o <- l1logit(train_set, test_set)
#l1logit.cv <- l1logit.o$cv
#l1logit.p <- l1logit.o$p
#l1logit.roc <- get_perf(l1logit.p)
#l1logit.fr <- get_perf(l1logit.p, "fr")
#l1logit.train.cv <- cv.glmnet(train_data[,-1], train_data[,1],
#family="binomial", type.measure="auc", nfolds=5)
#l1logit.pred <- predict(l1logit.train.cv, test_data_x, s="lambda.min", type="response")
#submit(l1logit.pred, "l1logit.csv")
# 0.821825 
#l1logit.sub <- l1logit(sub_train_set, sub_test_set)
#l1logit.sub.p <- l1logit.sub$p
#l1logit.sub.roc <- get_perf(l1logit.sub.p)

# Naive Bayes
nb <- function(train_set, test_set) {
  train <- naiveBayes(train_set[ ,-1], train_set[ ,1])
  fitted <- predict(train, test_set[ ,-1], type="raw")[,2]
  p <- prediction(fitted, test_set[ ,1])
  return(list(pred=fitted, p=p))
}
#nb.p <- nb(train_set, test_set)$p
#nb.roc <- get_perf(nb.p)
#nb.fr <- get_perf(nb.p, "fr")
#nb.fit <- naiveBayes(train_data[ ,-1], train_data[ ,1])
#nb.pred <- predict(nb.fit, test_data_x, type="raw")[,2] # 0.809185
#submit(nb.pred, "nb.csv")
#l1logit.nb.avg <- rowMeans(cbind(l1logit.pred, nb.pred))
#submit(l1logit.nb.avg, "l1logit_nb_avg.csv")

# Comparison
# ROC
#plot(l1logit.roc, main="Performance on Hold Out Data Set")
#lines(nb.roc@x.values[[1]], nb.roc@y.values[[1]], col="red")
#legend(0.4,0.2, c("L1 Regularized Logistic Regression", "Naive Bayes"),
#lty=c(1,1), col=c("black", "red"))
