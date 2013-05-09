library(R2jags)

source("data_prep_jags.R")

y <- train_data[,1]
x1 <- train_data[,3]
x2 <- train_data[,4]
x3 <- train_data[,10]
x4 <- train_data[,8]
x5 <- train_data[,13]
n <- length(y)
a1 <- 1
b1 <- 4
a2 <- 3
b2 <- 50
a3 <- 9
b3 <- 100
mu <- 52
s <- 15

jags.data <- list("y","x1","x2","x3","x4","x5","n","a1","b1","a2","b2","a3","b3")
jags.params <- c("q","lambda","lambda1", "lambda2","lambda3","b0","b[1]","b[2]","b[3]","b[4]","b[5]","mu","s")

jagsmodel <- function() {
  # Likelihood
  for (i in 1:n) {
    y[i] ~ dbern(p[i])
    x1[i] ~ dnorm(mu, s)
    x2[i] ~ dpois(lambda1)
    x3[i] ~ dpois(lambda2)
    x4[i] ~ dpois(lambda3)
    x5[i] ~ dbern(q)
    logit(p[i]) <- b0 + b[1]*x1[i] + b[2]*x2[i] + b[3]*x3[i] + b[4]*x4[i] + b[5]*x5[i]
  }
  # Prior on constant term
  b0 ~ dnorm(0,0.1)
  q ~ dunif(0,1)
  mu ~ dunif(40,60)
  s ~ dunif(10,20)
  lambda1 ~ dgamma(a1, b1)
  lambda2 ~ dgamma(a2, b2)
  lambda3 ~ dgamma(a3, b3)
  # L1 regularization == a Laplace (double exponential) prior 
  for (j in 1:5) {
    b[j] ~ ddexp(0, lambda)  
  }
  lambda ~ dunif(0.001,10)
}

#fit <- jags.parallel(data=jags.data, parameters.to.save=jags.params, n.chains=8, n.iter=10000,n.burnin=5000, model.file=jagsmodel)
