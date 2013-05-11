library(rstan)

source("data_prep_jags.R")

#y <- train_data[,1]
#x1 <- train_data[,3]
#x2 <- train_data[,4]
#x3 <- train_data[,10]
#x4 <- train_data[,8]
#x5 <- train_data[,13]
y <- sub_train_data[,1]
x1 <- sub_train_data[,3]
x2 <- sub_train_data[,4]
x3 <- sub_train_data[,10]
x4 <- sub_train_data[,8]
x5 <- sub_train_data[,13]
n <- length(y)

model <- '
  data {
    int<lower=0> n;
    int<lower=0,upper=1> y[n]; 
    real x1[n]; 
    int<lower=0> x2[n];
    int<lower=0> x3[n];
    int<lower=0> x4[n];
    int<lower=0,upper=1> x5[n];
  }
  parameters {
    real mu; 
    real<lower=0.0001> s;
    real b[6];
    real<lower=0,upper=1> q;
    real<lower=0> lambda1;
    real<lower=0> lambda2;
    real<lower=0> lambda3;
    real<lower=0> lambda;
  }
  model {
    for (i in 1:n) {
      y[i] ~ bernoulli(inv_logit(b[1]+b[2]*x1[i]+b[3]*x2[i]+b[4]*x3[i]+b[5]*x4[i]+b[6]*x5[i]));
      x1[i] ~ normal(mu,s);
      x2[i] ~ poisson(lambda1);
      x3[i] ~ poisson(lambda2);
      x4[i] ~ poisson(lambda3);
      x5[i] ~ bernoulli(q);
    }
    lambda1 ~ uniform(0,10);
    lambda2 ~ uniform(0,10);
    lambda3 ~ uniform(0,10);
    q ~ uniform(0,1);
    mu ~ uniform(40,60);
    s ~ uniform(10,20);
    for (i in 1:6) {
      b[i] ~ double_exponential(0,lambda);
    }
    lambda ~ uniform(0.001,10);
  }
'

#credit_data <- list(y=y,n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,a1=a1,b1=b1,a2=a2,b2=b2,a3=a3,b3=b3)
credit_data <- list(y=y,n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5)

init_fun <- function(chain_id) {
  if (chain_id==1) {
    mu <- 52
    s <- 15
    q <- 0.5
    lambda1 <- 0.1
    lambda2 <- 0.1
    lambda3 <- 0.1
    lambda <- 0.001
    b <- c(0.1,0.1,0.1,0.1,0.1,0.1)
  }
  else if (chain_id==2) {
    mu <- 10
    s <- 5
    q <- 0.3
    lambda1 <- 0.001
    lambda2 <- 0.001
    lambda3 <- 0.001
    lambda <- 0.005
    b <- c(0.001,0.1,0.2,0.3,0.4,0.5)
  }
  else if (chain_id==3) {
    mu <- 90
    s <- 20
    q <- 0.7
    lambda1 <- 1
    lambda2 <- 2
    lambda3 <- 3
    lambda <- 0.1
    b <- c(-0.1,-0.1,-0.1,-0.1,-0.1,-0.1)
  }
  else if (chain_id==4) {
    mu <- 70
    s <- 15
    q <- 0.9
    lambda1 <- 0.1
    lambda2 <- 0.1
    lambda3 <- 0.1
    lambda <- 0.001
    b <- c(0.1,0.1,0.1,0.1,0.1,0.1)
  }
  else if (chain_id==5) {
    mu <- 52
    s <- 15
    q <- 0.1
    lambda1 <- 0.1
    lambda2 <- 0.1
    lambda3 <- 0.1
    lambda <- 0.001
    b <- c(-0.001,-0.1,-0.2,-0.3,-0.4,-0.5)
  }
  else if (chain_id==6) {
    mu <- 52
    s <- 15
    q <- 0.5
    lambda1 <- 10
    lambda2 <- 15
    lambda3 <- 20
    lambda <- 5
    b <- c(1,5,10,15,25,30)
  }
  else if (chain_id==7) {
    mu <- 52
    s <- 15
    q <- 0.5
    lambda1 <- 0.1
    lambda2 <- 0.1
    lambda3 <- 0.1
    lambda <- 0.001
    b <- c(-1,-5,-10,-15,-25,-30)
  }
  else if (chain_id==8) {
    mu <- 52
    s <- 15
    q <- 0.5
    lambda1 <- 0.9
    lambda2 <- 0.5
    lambda3 <- 0.3
    lambda <- 0.000001
    b <- c(0.001,0.001,0.001,0.001,0.001,0.001)
  }
  return(list(mu=mu,s=s,q=q,lambda1=lambda1,lambda2=lambda2,lambda3=lambda3,lambda=lambda,b=b))
}

f1 <- stan(model_code = model, data = credit_data, init=init_fun, iter = 1, chains = 1)
#chains <- 8
#posterior <- mclapply(1:chains, mc.cores = chains, FUN = function(chain) {
#  stan(fit = f1, data = credit_data, chains = 1, refresh = -1)})
#posterior.fit <- sflist2stanfit(posterior)
