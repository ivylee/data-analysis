# a.Prepare analysis of the type we did for Bioassay problem, 
# with the covariate being the temperature and the response variable being 
# #of primary O-rings in field joints with Erosion or blowby. 
# There are 6 primary O-rings per shuttle. 
# Challenger was launched at temperature of 31 degrees.
input<-function(LL=200){
  oring<-data.frame(temp <- c(66,70,69,68,67,72,73,70,57,63,70,78,
                              67,53,67,75,70,81,76,79,75,76,58),
                    orings<-rep(6,length(temp)),
                    eorb<-c(0.5,1,0.5,0.5,0.5,0.5,0.5,0.5,1,1,1,
                            0.5,0.5,2,0.5,0.5,0.5,0.5,0.5,0.5,2,0.5,1),
                    freq<-eorb/6)
  DD<-data.frame(y<-log((oring$freq)/(1- oring$freq)),x<-oring$temp)
  estimates <- lm(y~x,data=DD)
  alpha.hat <- summary(estimates)$coef[1,1]
  std.alpha <- summary(estimates)$coef[1,2]
  beta.hat <- summary(estimates)$coef[2,1]
  std.beta <- summary(estimates)$coef[2,2]
  alpha<-seq(-2,3,length=LL)
  beta<-seq(-1,0,length=LL)
  return(list(oring=oring,estimates=estimates,alpha=alpha,beta=beta))
}

log.post<-function(alpha,beta,data=DD$oring){
  ldens<-0
  for (i in 1:length(data$temp)){
    theta <- 1/(1+exp(-alpha-beta*data$temp[i]))
    ldens <- ldens + data$eorb[i]*log(theta) + 
    (data$orings[i]-data$eorb[i])*log(1-theta)}
  return(ldens)
}

randomdraw2d<-function(size,x,y,prob){
  if (sum(prob)!=1){
    prob<-prob/sum(prob)
  }
  probx<-rowSums(prob)
  xsampleindex<-sample(1:length(x),size=size,replace=TRUE,prob=probx)
  ysampleindex<-rep(NA,size)
  for (i in 1:size){
    proby.x<-prob[xsampleindex[i],]
    proby.x<-proby.x/sum(proby.x)
    ysampleindex[i]<-sample(1:length(y),1,replace=TRUE,prob=proby.x)
  }
  sample2d<-cbind(x[xsampleindex],y[ysampleindex])
  return(sample2d)
}

plot.joint.post=function(DD,log.post,drawsize){
  contours=seq(.05,.95,.1)
  logdens=outer(DD$alpha,DD$beta,log.post,data=DD$oring)
  dens=exp(logdens-max(logdens))
  contour(DD$alpha,DD$beta,dens,levels=contours,
          xlab= "alpha", ylab="beta", ylim=c(-0.1,0))
  points(randomdraw2d(drawsize,DD$alpha,DD$beta,prob= dens/sum(dens)),pch=".")
  mtext("Posterior density",3,line=1,cex=1.2)
}

DD <- input(200)
prob <- outer(DD$alpha, DD$beta, log.post, DD$oring)
prob <- exp(prob)
prob <- prob/sum(prob)
pdraws <- randomdraw2d(10000,DD$alpha, DD$beta, prob)

# b.Compute the predictive distribution of an erosion or blowby at 31 degrees
theta31 <- 1/(1+exp(-(pdraws[,1]+pdraws[,2]*31)))
pdf("dist31.pdf")
hist(theta31,freq=FALSE,breaks=30, yaxt="n", 
     xlab="Predictive dist. at 31 degrees", ylab="", main="")
lines(density(theta31))
dev.off()

# c.Find distribution of x such that p(erosion or blowby)=.05 based on this data
ld50 <- -pdraws[,1]/pdraws[,2]
pdf("ld50.pdf")
hist(ld50,freq=FALSE,breaks=30, yaxt="n", xlab="LD50", ylab="", main="")
lines(density(ld50))
dev.off()

# d.What is the credible interval for the probability of erosion/blowby 
# at 31 degrees Fahrenheit?
c2 <- quantile(theta31,0.975)
c1 <- quantile(theta31,0.025)
# 95% credible interval: [0.1267895, 0.6689950]
ci95 <- c(c1,c2)

# e.Find the predictive distribution of probability of a field joint O-ring being 
# damaged (erosion/blowby) to a new space shuttle launch at 50 degrees
theta50 <- 1/(1+exp(-(pdraws[,1]+pdraws[,2]*50)))
pdf("dist50.pdf")
hist(theta50,freq=FALSE,breaks=30, yaxt="n", 
     xlab="Predictive dist. at 50 degrees", ylab="", main="")
lines(density(theta50))
dev.off()

# f.Suppose you were told that an expert has predicted that the space shuttle at 
# temperature of 60 degrees will have has median .02 and 90 percentile of .08. 
# How would you quantify this in your prior? How would your posterior change?
log.post.betaprior<-function(alpha,beta,data=DD$oring){
  params <- beta.select(list(p=.5,x=0.02),list(p=.9,x=0.08))
  ldens <- (alpha+60*beta)*(params[1]-1)+
  (2-params[1]-params[2])*log(1+exp(alpha+60*beta))
  for (i in 1:length(data$temp)){
    theta <- 1/(1+exp(-alpha-beta*data$temp[i]))
    ldens <- ldens + data$eorb[i]*log(theta) + 
    (data$orings[i]-data$eorb[i])*log(1-theta)}
  return(ldens)
}

pdf("post_dist_1.pdf")
plot.joint.post(DD,log.post,drawsize=1000)
dev.off()

pdf("post_dist_beta.pdf")
plot.joint.post(DD,log.post.betaprior,drawsize=1000)
dev.off()
