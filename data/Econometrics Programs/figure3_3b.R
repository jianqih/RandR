#########################################
### This file generates Figure 3.3b,
### Sum-of-Squared Errors Function
### as a function of beta1,beta2
#########################################
### Uses packages lattice, MASS
#########################################

library(lattice) # package for 3D-plot
library(MASS)    # package for mvrnorm 

### generate function for sse
n <- 100
set.seed(3)
mu <- c(0,0)
sigma <- matrix(c(1/4,-1/8,-1/8,1/4),2)
x<-mvrnorm(n,mu,sigma)
e <- 2*rnorm(n)
x1 <- x[,1]
x2 <- x[,2]
y <- x1*3 + x2*3 + e

sse <- function (b1,b2) {
  yfit <- x1*b1+x2*b2
  val <- sum((y-yfit)^2)
  return(val)
}
v_sse <- Vectorize(sse)

### draw contour (Figure 3.2c)

b1 <- seq(2,4,0.005)
b2 <- seq(2,4,0.005)
z <- outer(b1,b2,FUN=v_sse)
ind_b <- which(z==min(z),arr.ind=TRUE)
min_b1 <- b1[ind_b[1]]
min_b2 <- b2[ind_b[2]]

lwd <- 1.4

pdf("HANSEN3-3b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
par(mar=c(5.1,5.1,4.1,2.1))
contour(b1,b2,z,levels=c(415,417,419,421,423),drawlabels=FALSE,xaxs="i",yaxs="i",xlim=c(2.3,4),ylim=c(2.2,3.8),
        xlab=expression(beta[1]),ylab=expression(beta[2]),xaxt="n", yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(2,4,.5),lwd=wd)
axis(side=2,seq(2,4,.5),lwd=wd)
points(min_b1,min_b2,pch=20)
text(min_b1+0.05,min_b2,labels=expression(hat(beta)))
dev.off()

postscript("HANSEN3-3b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
par(mar=c(5.1,5.1,4.1,2.1))
contour(b1,b2,z,levels=c(415,417,419,421,423),drawlabels=FALSE,xaxs="i",yaxs="i",xlim=c(2.3,4),ylim=c(2.2,3.8),
        xlab=expression(beta[1]),ylab=expression(beta[2]),xaxt="n", yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(2,4,.5),lwd=wd)
axis(side=2,seq(2,4,.5),lwd=wd)
points(min_b1,min_b2,pch=20)
text(min_b1+0.05,min_b2,labels=expression(hat(beta)))
dev.off()