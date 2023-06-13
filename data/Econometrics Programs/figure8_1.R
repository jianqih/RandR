#########################################
### This file generates Figure 8.1,
### Sum-of-Squared Errors Function
### as a function of beta1,beta2
### with constrained least squares example 
#########################################
### Uses package MASS
#########################################

library(MASS)

### generate function for sse
n <- 100
set.seed(3)
mu <- c(0,0)
sigma <- matrix(c(1/4,1/8,1/8,1/4),2)
x<-mvrnorm(n,mu,sigma)
e <- rnorm(n)
x1 <- x[,1]
x2 <- x[,2]
y <- x1*1/3 + x2*1/3 + e

sse <- function (b1,b2) {
  yfit <- x1*b1+x2*b2
  val <- sum((y-yfit)^2)
  return(val)
}
v_sse <- Vectorize(sse)

### draw contour 

b1 <- seq(0,1,0.005)
b2 <- seq(0,1,0.005)
z <- outer(b1,b2,FUN=v_sse)
ind_b <- which(z==min(z),arr.ind=TRUE)
min_b1 <- b1[ind_b[1]]
min_b2 <- b2[ind_b[2]]

### obtain cls
b_ols <- c(min_b1,min_b2)
xx <- solve(t(x)%*%x)
R <- c(1,1)
C <- 1
b_cls <- b_ols - xx%*%R%*%solve(t(R)%*%xx%*%R)%*%(R%*%b_ols-C)
b_cls1 <- b_cls[1]
b_cls2 <- b_cls[2]
sse_cls <- sse(b_cls1,b_cls2)

pdf("HANSEN8-1.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
oldpar <- par(mar=c(5.1,5.1,4.1,2.1))
contour(b1,b2,z,levels=c(103.8,104.2,104.6,105,105.4,105.8),drawlabels=FALSE,xaxs="i",yaxs="i",
        xlab=expression(beta[1]),ylab=expression(beta[2]),bty='l',cex.lab=.8,cex.axis=.75)
points(min_b1,min_b2,pch=20)
text(min_b1+0.06,min_b2,labels=expression(hat(beta)[ols]),cex=.8)
points(b_cls1,b_cls2,pch=20,cex=.8)
text(b_cls1+0.05,b_cls2+0.02,labels=expression(tilde(beta)[cls]),cex=.8)
abline(a=1,b=-1)
par(oldpar)
dev.off()

postscript("HANSEN8-1.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
oldpar <- par(mar=c(5.1,5.1,4.1,2.1))
contour(b1,b2,z,levels=c(103.8,104.2,104.6,105,105.4,105.8),drawlabels=FALSE,xaxs="i",yaxs="i",
        xlab=expression(beta[1]),ylab=expression(beta[2]),bty='l',cex.lab=.8,cex.axis=.75)
points(min_b1,min_b2,pch=20)
text(min_b1+0.06,min_b2,labels=expression(hat(beta)[ols]),cex=.8)
points(b_cls1,b_cls2,pch=20,cex=.8)
text(b_cls1+0.05,b_cls2+0.02,labels=expression(tilde(beta)[cls]),cex=.8)
par(oldpar)
dev.off()
