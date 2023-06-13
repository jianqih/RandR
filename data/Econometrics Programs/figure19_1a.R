#########################################################################
##  This file generates Figure 19.1a
##  Nadaraya-Watson Regression
#########################################################################


n <- 100
xm = 10
x <- seq(0,xm,.01)
xn <- length(x)
set.seed(180)
xdat <- runif(n,0,xm)
a <- pi/4
m <- sin((xdat-2)*a)/((xdat-2)*a)
ydat <- m + rnorm(n)/4

h1 <- 1
h2 = h1/sqrt(3)

x0 <- seq(1,9,2)
x0n = length(x0)
k0 <- (abs(x0%*%matrix(1,1,n)-matrix(1,x0n,1)%*%t(xdat)) <= h1)
m0 <- (k0%*%ydat)/rowSums(k0)

x1 <- seq(0,2,.01)
hist1 <- matrix(m0[1],201,1)
x2 <- seq(2,4,.01)
hist2 <- matrix(m0[2],201,1)
x3 <- seq(4,6,.01)
hist3 <- matrix(m0[3],201,1)
x4 <- seq(6,8,.01)
hist4 <- matrix(m0[4],201,1)
x5 <- seq(8,10,.01)
hist5 <- matrix(m0[5],201,1)

k1 <- (abs(x%*%matrix(1,1,n)-matrix(1,xn,1)%*%t(xdat)) <= h1)
m1 <- (k1%*%ydat)/rowSums(k1)

k2 <- dnorm(abs(x%*%matrix(1,1,n)-matrix(1,xn,1)%*%t(xdat))/h2)
m2 <- (k2%*%ydat)/rowSums(k2)

wd <- 1.4

pdf("HANSEN19-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,m1,type="l",lty=1,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",xlim=c(0,10),ylim=c(-.75,1.5),xaxt="n",yaxt="n",lwd=wd,bty="n")
lines(x,m2,lty=5,lwd=wd)
points(xdat,ydat,pch=1,cex=.7)
points(x0,m0,pch=15,cex=1)
lines(x1,hist1,lty=2,lwd=wd)
lines(x2,hist2,lty=2,lwd=wd)
lines(x3,hist3,lty=2,lwd=wd)
lines(x4,hist4,lty=2,lwd=wd)
lines(x5,hist5,lty=2,lwd=wd)
abline(v=2,lty=3,lwd=wd)
abline(v=4,lty=3,lwd=wd)
abline(v=6,lty=3,lwd=wd)
abline(v=8,lty=3,lwd=wd)
axis(side=1,seq(0,10,1),lwd=wd)
axis(side=2,seq(-1,2,.5),lwd=wd)
legend("topright",legend=c("Binned Means","Rolling Binned","Nadaraya-Watson"),lty=c(2,1,5),bg="white",lwd=wd,bty="n")
dev.off()

postscript("HANSEN19-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,m1,type="l",lty=1,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",xlim=c(0,10),ylim=c(-.75,1.5),xaxt="n",yaxt="n",lwd=wd,bty="n")
lines(x,m2,lty=5,lwd=wd)
points(xdat,ydat,pch=1,cex=.7)
points(x0,m0,pch=15,cex=1)
lines(x1,hist1,lty=2,lwd=wd)
lines(x2,hist2,lty=2,lwd=wd)
lines(x3,hist3,lty=2,lwd=wd)
lines(x4,hist4,lty=2,lwd=wd)
lines(x5,hist5,lty=2,lwd=wd)
abline(v=2,lty=3,lwd=wd)
abline(v=4,lty=3,lwd=wd)
abline(v=6,lty=3,lwd=wd)
abline(v=8,lty=3,lwd=wd)
axis(side=1,seq(0,10,1),lwd=wd)
axis(side=2,seq(-1,2,.5),lwd=wd)
legend("topright",legend=c("Binned Means","Rolling Binned","Nadaraya-Watson"),lty=c(2,1,5),bg="white",lwd=wd,bty="n")
dev.off()
