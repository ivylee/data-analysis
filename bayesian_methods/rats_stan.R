library(rstan)
library(ggplot2)

rats_exp <- read.table("rats.asc", header=T)
names(rats_exp) <- c("y", "N")
rats_data <- list(J = length(rats_exp$y), y=rats_exp$y, n=rats_exp$N)
fit <- stan(file = "rats.stan", data=rats_data, iter=1000, chain=4)
print(fit)
pdf("rats_stan_plots.pdf")
plot(fit)
dev.off()
theta <- extract(fit, permuted=T)$theta
theta_u <- rep(NA, length(theta[1, ]))
theta_l <- rep(NA, length(theta[1, ]))
for (i in 1:length(theta[1, ])) {
  theta_u[i] <- quantile(theta[ ,i], 0.975)
  theta_l[i] <- quantile(theta[ ,i], 0.025)
}
theta_mean <- colMeans(theta)
obs_rate <- rats_data$y/rats_data$n
df <- data.frame(obs_rate, theta_mean, theta_u, theta_l)
pdf("rats_post_theta.pdf")
ggplot(df, aes(x=obs_rate, y=theta_mean)) + geom_point() +
geom_errorbar(aes(ymax=theta_u, ymin=theta_l))
dev.off()
alpha <- extract(fit, permuted=T)$alpha
beta <- extract(fit, permuted=T)$beta
pdf("rats_post_alpha.pdf")
plot(density(alpha))
dev.off()
pdf("rats_post_beta.pdf")
plot(density(beta))
dev.off()
