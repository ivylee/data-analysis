hospital.data<-function() {
  operations<-c(47,148,119,810,211,196,148,215,207,97,256,360)
  deaths<-c(0,18,8,46,8,13,9,31,14,8,29,24)
  data<-data.frame(operations,deaths)
  return(data)
}

grid<-function(alpha_lb,alpha_ub,beta_lb,beta_ub, length) {
  alpha<-seq(alpha_lb, alpha_ub, length=length)
  beta<-seq(beta_lb, beta_ub, length=length)
  parameter<-data.frame(alpha, beta)
  return(parameter)
}

log.post.ab<-function(alpha,beta,data) {
  log.post<-(-5/2)*log(alpha+beta)
  for (i in 1:length(data$deaths)) {
    log.post<-log.post+log(beta(alpha+data$deaths[i],beta+data$operations[i]-
    data$deaths[i]))-log(beta(alpha,beta))
  }
  return(log.post)
}

randomdraw2d<-function(size,x,y,prob){
  if (sum(prob)!=1) {
    prob<-prob/sum(prob)
  }
  probx<-rowSums(prob)
  xsampleindex<-sample(1:length(x),size=size,replace=TRUE,prob=probx)
  ysampleindex<-rep(NA,size)
  for (i in 1:size) {
    proby.x<-prob[xsampleindex[i],]
    proby.x<-proby.x/sum(proby.x)
    ysampleindex[i]<-sample(1:length(y),1,replace=TRUE,prob=proby.x)
  }
  sample2d=cbind(x[xsampleindex],y[ysampleindex])
  return(sample2d)
}

plot.post.ab<-function(alpha,beta,data) {
  pdf("hospital_posterior_alpha_beta.pdf")
  contours=seq(0.05,0.95,0.1)
  log.posterior<-outer(alpha,beta,log.post.ab,data)
  dens<-exp(log.posterior-max(log.posterior))
  contour(alpha,beta,dens,levels=contours,xlab="alpha",ylab="beta",cex=1)
  posterior.sample<-randomdraw2d(1000,alpha,beta,prob=dens/sum(dens))
  points(posterior.sample,pch=".")
  dev.off()
  return(posterior.sample)
}

posterior.theta<-function(alpha,beta,data) {
  log.posterior<-outer(alpha,beta,log.post.ab,data)
  dens<-exp(log.posterior-max(log.posterior))
  posterior.sample<-randomdraw2d(1000,alpha,beta,prob=dens/sum(dens))
  post.theta<-vector()
  for (i in 1:length(posterior.sample[ ,1])) {
    sample.alpha<-posterior.sample[i, 1]
    sample.beta<-posterior.sample[i, 2]
    thetas<-vector()
    for (j in 1:length(data$deaths)) {
      theta<-rbeta(1, sample.alpha+data$deaths[j], 
      sample.beta+data$operations[j]-data$deaths[j])
      thetas<-c(thetas, theta)
    }
    post.theta<-cbind(post.theta, thetas)
  }
  post.theta<-as.data.frame(t(post.theta), row.names=1)
  colnames(post.theta)<-
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
# Compare the marginal distributions of theta by boxplot.
  pdf("hospital_boxplot_theta.pdf")
  boxplot(as.list(post.theta))
  dev.off()
# Find the 90% marginal credible intervals for theta
  cred.int<-vector()
  for (i in 1:length(post.theta[1, ])) {
    ci<-quantile(post.theta[ ,i], c(0.05, 0.95))
    cred.int<-rbind(cred.int, ci)
  }
# Find the posterior distribution of theta_A-theta_H
  prob.ah<-post.theta$A-post.theta$H
  prob.ah<-prob.ah/sum(prob.ah)
  pdf("hospital_posterior_ah.pdf")
  plot(density(prob.ah))
  dev.off()
# If you had to go for a surgery, where would you go? metric=min(mean^2+std^2)
  m<-vector()
  for (i in 1:length(post.theta[1, ])) {
    mean<-mean(post.theta[ ,i])
    std<-sd(post.theta[ ,i])
    measure<-mean^2+std^2
    m<-c(m, measure)
  }
  best<-min(m)
  hospital<-which(m==best,arr.in=T)
  return(list(posterior.theta.dist=post.theta, 
  credible.interval=cred.int, 
  posterior.thetaA_thetaH=prob.ah, best.choice=hospital))
}

data<-hospital.data()
param<-grid(0.01, 15, 0.01, 122, 200)
alpha<-param$alpha
beta<-param$beta
post.ab.sampl<-plot.post.ab(alpha, beta, data)
post.theta.result<-posterior.theta(alpha, beta, data)
post.theta.result$credible.interval
         5%        95%
0.009967567 0.08453421
0.075008542 0.14866963
0.041804600 0.10463566
0.045710072 0.07034051
0.027544208 0.06897690
0.045878029 0.09779177
0.040009368 0.09793855
0.096679254 0.16959154
0.046640636 0.09504442
0.048138353 0.12161549
0.079943327 0.13599924
0.049051921 0.09075379
post.theta.result$best.choice
5 # hospital E
