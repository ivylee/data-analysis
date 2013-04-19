data <- read.table("change_point_data.txt")
colnames(data) <- c("time", "incid")

#(a, b) are parameters for prior of lambda,  and,  (g, d) are parameters for phi
#theta.matrix is the input with initial values being 
#theta.matrix[1, ] <- c(lambda, phi, k)

bcp <- function(theta.matrix, data, a, b, g, d) {
  n <- length(data$time)
  n1 <- 0
  n2 <- 0
  for (i in 1:n) {
    if (data$time[i] == 1) {
      n1 <- n1 + 1
      n2 <- n2 + 1
    }
    else if (data$time[i] ==  2) {
      n2 <- n2 + 1
    }
  }
  obs <- c(n1, n2, n)
  k.prob <- rep(0, 3)
  lambda <- theta.matrix[1, 1]
  phi <- theta.matrix[1, 2]
  for (i in 2:nrow(theta.matrix)) {
    lambda <- rgamma(1, a + sum(data$incid[1:obs[theta.matrix[i-1, 3]]]),
    b + obs[theta.matrix[i-1, 3]])
    phi <- rgamma(1, g + sum(data$incid[obs[theta.matrix[i-1, 3]]:n]),
    d + n-obs[theta.matrix[i-1, 3]])
    for (j in 1:3) {
      k.prob[j] <-
      exp(obs[j]*(phi-lambda))*lambda^sum(data$incid[1:obs[j]])*
      phi^sum(data$incid[obs[j]:n])
    }
    k.prob <- k.prob/sum(k.prob)
    k <- sample(1:3, size=1, prob=k.prob)
    theta.matrix[i, ] <- c(lambda, phi, k)
  }
  return(theta.matrix)
}

nsim <- 1000
theta.matrix <- matrix(NA, nrow <- nsim, ncol <- 3)
colnames(theta.matrix) <- c("lambda", "phi", "k")
theta.matrix[1, ] <- c(1, 2, 1) #initial values
theta.sim <- bcp(theta.matrix, data, a=4, b=1, g=2, d=3)

plot.walk <- function(walk.mat, X, Y) {
  plot(walk.mat[1, X], walk.mat[1, Y], type="n", 
  xlim=range(walk.mat[ ,X]), ylim=range(walk.mat[ ,Y]), xlab="", ylab="")
  for (i in 1:(nrow(walk.mat)-1)){
    segments(walk.mat[i, X], walk.mat[i, Y], 
    walk.mat[i + 1, X], walk.mat[i, Y])
    segments(walk.mat[i + 1, X], walk.mat[i, Y], 
    walk.mat[i + 1, X], walk.mat[i + 1, Y])
  }
}

par(mfrow=c(1, 2), mar=c(2, 3, 1, 1), oma=c(1, 1, 3, 1))
plot.walk(theta.sim[1:100, ], X=1, Y=3)
mtext(outer=TRUE, side=2, cex=1.3, "k")
mtext(outer=FALSE, side=3, cex=1.3, expression(lambda), line=2)
plot.walk(theta.sim[1:100, ], X=2, Y=3)
mtext(outer=TRUE, side=2, cex=1.3, "k")
mtext(outer=FALSE, side=3, cex=1.3, expression(phi), line=2)

pdf("cp_k_iter.pdf")
plot(theta.sim[,3], xlab="Iteration", ylab="k")
dev.off()
