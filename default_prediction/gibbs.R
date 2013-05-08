library(R2jags)

source("data_prep.R")
train <- clean(train)
test <- clean(test)

y <- train[ ,1]
x1 <- train$age
x2 <- train$
a1 <- 1
b1 <- 1
a2 <- 1
b2 <- 1
a3 <- 1
b3 <- 1
n <- length(Y)

jags.data <- list("Y","a1","b1","a2","b2","n")
jags.params <- c("q", "lambda1", "lambda2")
jags.inits <- function() {
  list("q"=runif(1), "lambda1"=runif(1, min=3, max=5), "lambda2"=runif(1,
  min=6, max=9))
}

#jagsmodel<- function() {
#   for (i in 1:n) {
#     Z[i] ~ dbern(q)
#     Y[i] ~ dpois(lambda1*Z[i]+lambda2*(1-Z[i]))
#   }
#   q ~ dunif(0,1)
#   lambda1 ~ dgamma(a1, b1)
#   lambda2 ~ dgamma(a2, b2)
#}

fit <- jags(jags.data, jags.inits, jags.params, n.chains=4, n.iter=10000,
n.burnin=5000, model.file=jagsmodel)

jagsmodel <- function() {
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbern(p[i])
    x1[i] ~ dnorm(52, 15)
    x2[i] ~ dpois(lambda1)
    x3[i] ~ dpois(lambda2)
    x4[i] ~ dpois(lambda3)
    x5[i] ~ dbern(q)
    logit(p[i]) <- b0 + b[1]*x1[i] + b[2]*x2[i] + b[3]*x3[i] + b[4]*x4[i] + b[5]*x5[i]
  }
  # Prior on constant term
  b0 ~ dnorm(0,0.1)
  q ~ dunif(0,1)
  lambda1 ~ dgamma(a1, b1)
  lambda2 ~ dgamma(a2, b2)
  lambda3 ~ dgamma(a3, b3)
  # L1 regularization == a Laplace (double exponential) prior 
  for (j in 1:5) {
    b[j] ~ ddexp(0, lambda)  
  }
  lambda ~ dunif(0.001,10)
}
