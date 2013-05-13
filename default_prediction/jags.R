######################################### 
# Title: JAGS code for hierarchical model
# Author: Ivy Lee
# Date: 12 May 2013
#########################################

library(R2jags)

source("data_prep_jags.R")

sub_train_data<-subsample(train_data,10000,0.5)

y <- sub_train_data[,1]
x1 <- sub_train_data[,2]
x2 <- sub_train_data[,3]
x3 <- sub_train_data[,4]
x4 <- sub_train_data[,5]
x5 <- sub_train_data[,6]
x6 <- sub_train_data[,7]
x7 <- sub_train_data[,8]
x8 <- sub_train_data[,9]
x9 <- sub_train_data[,10]
x10 <- sub_train_data[,11]
x11 <- sub_train_data[,12]
x12 <- sub_train_data[,13]
x13 <- sub_train_data[,14]
n <- length(y)

jags.data <- list("y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","n")
jags.params <- c("lambda","lambda1", "lambda2","lambda3","beta0","beta")

jagsmodel <- function() {
  # Likelihood
  for (i in 1:n) {
#    p[i] ~ dunif(0,1)
    y[i] ~ dbern(p[i])
    x3[i] ~ dpois(lambda1)
    x9[i] ~ dpois(lambda2)
    x7[i] ~ dpois(lambda3)
    logit(p[i]) <- beta0 + beta[1]*x1[i] + beta[2]*x2[i] + beta[3]*x3[i] + beta[4]*x4[i] + beta[5]*x5[i] + beta[6]*x6[i] + beta[7]*x7[i] + beta[8]*x8[i] + beta[9]*x9[i] + beta[10]*x10[i] + beta[11]*x11[i] + beta[12]*x12[i] + beta[13]*x13[i]
  }
  # L1 regularization == a Laplace (double exponential) prior 
  for (i in 1:13) {
    beta[i] ~ ddexp(0, lambda)
  }  
  beta0 ~ dnorm(0, 1)
  lambda ~ dunif(0.0001,10)
  lambda1 ~ dunif(0,10)
  lambda2 ~ dunif(0,10)
  lambda3 ~ dunif(0,10)
}

#fit <- jags.parallel(data=jags.data, parameters.to.save=jags.params, n.chains=8, n.iter=10000,n.burnin=5000, model.file=jagsmodel)
#jagsfit <- jags(data=jags.data, parameters.to.save=jags.params, n.iter=10, model.file=jagsmodel)
