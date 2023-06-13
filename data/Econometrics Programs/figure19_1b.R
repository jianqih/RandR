#########################################################################
##  This file generates Figure 19.1b
##  Local Linear Regression
#########################################################################


n <- 100
xm = 10
x <- seq(0,xm,.01)
xn <- length(x)
set.seed(180)
xdat <- runif(n,0,xm)
a <- pi/4
m <- sin(xdat*a)/(xdat*a)
m <- sin((xdat-2)*a)/((xdat-2)*a)
ydat <- m + rnorm(n)/4

h1 <- 1
h2 <- h1/sqrt(3)

mg <- matrix(0,xn,1)
mr <- matrix(0,xn,1)
for (j in 1:xn){
  xj <- xdat-x[j]
  z <- cbind(matrix(1,n,1),xj)
  k1 <- abs(xj) < h1
  k2 <- dnorm(xj/h2)
  zk1 <- z*k1
  zk2 <- z*k2
  betar <- solve(t(zk1)%*%z,t(zk1)%*%ydat)
  betag <- solve(t(zk2)%*%z,t(zk2)%*%ydat)
  mr[j] <- betar[1]
  mg[j] <- betag[1]
}

x0 <- c(1,3,5,7,9)

x1 <- as.matrix(subset(xdat,xdat<2))
y1 <- as.matrix(subset(ydat,xdat<2))
z1 <- cbind(matrix(1,length(y1),1),x1)
beta1 <- solve(t(z1)%*%z1,t(z1)%*%y1)
m1 <- beta1[1]+beta1[2]*x0[1]
x1 <- as.matrix(subset(x,x<2))
f1 <- beta1[1]+x1*beta1[2]

x2 <- as.matrix(subset(xdat,(xdat>=2)&(xdat<4)))
y2 <- as.matrix(subset(ydat,(xdat>=2)&(xdat<4)))
z2 <- cbind(matrix(1,length(y2),1),x2)
beta2 <- solve(t(z2)%*%z2,t(z2)%*%y2)
m2 <- beta2[1]+beta2[2]*x0[2]
x2 <- as.matrix(subset(x,(x>=2)&(x<4)))
f2 <- beta2[1]+x2*beta2[2]

x3 <- as.matrix(subset(xdat,(xdat>=4)&(xdat<6)))
y3 <- as.matrix(subset(ydat,(xdat>=4)&(xdat<6)))
z3 <- cbind(matrix(1,length(y3),1),x3)
beta3 <- solve(t(z3)%*%z3,t(z3)%*%y3)
m3 <- beta3[1]+beta3[2]*x0[3]
x3 <- as.matrix(subset(x,(x>=4)&(x<6)))
f3 <- beta3[1]+x3*beta3[2]

x4 <- as.matrix(subset(xdat,(xdat>=6)&(xdat<8)))
y4 <- as.matrix(subset(ydat,(xdat>=6)&(xdat<8)))
z4 <- cbind(matrix(1,length(y4),1),x4)
beta4 <- solve(t(z4)%*%z4,t(z4)%*%y4)
m4 <- beta4[1]+beta4[2]*x0[4]
x4 <- as.matrix(subset(x,(x>=6)&(x<8)))
f4 <- beta4[1]+x4*beta4[2]

x5 <- as.matrix(subset(xdat,xdat>=8))
y5 <- as.matrix(subset(ydat,xdat>=8))
z5 <- cbind(matrix(1,length(y5),1),x5)
beta5 <- solve(t(z5)%*%z5,t(z5)%*%y5)
m5 <- beta5[1]+beta5[2]*x0[5]
x5 <- as.matrix(subset(x,(x>=8)))
f5 <- beta5[1]+x5*beta5[2]

m0 <- c(m1,m2,m3,m4,m5)

wd <- 1.4

pdf("HANSEN19-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,mr,type="l",lty=1,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",ylim=c(-.75,1.5),xlim=c(0,10),xaxt="n", yaxt="n",lwd=wd,bty="n")
lines(x,mg,lty=5,lwd=wd)
points(xdat,ydat,pch=1,cex=.7)
points(x0,m0,pch=15,cex=1)
abline(v=2,lty=3,lwd=wd)
abline(v=4,lty=3,lwd=wd)
abline(v=6,lty=3,lwd=wd)
abline(v=8,lty=3,lwd=wd)
lines(x1,f1,lty=2,lwd=wd)
lines(x2,f2,lty=2,lwd=wd)
lines(x3,f3,lty=2,lwd=wd)
lines(x4,f4,lty=2,lwd=wd)
lines(x5,f5,lty=2,lwd=wd)
axis(side=1,seq(0,10,1),lwd=wd)
axis(side=2,seq(-1,2,.5),lwd=wd)
legend("topright",legend=c("Binned Regression","Rolling Binned","Local Linear"),lty=c(2,1,5),bg="white",lwd=wd,bty="n")
dev.off()

postscript("HANSEN19-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,mr,type="l",lty=1,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",ylim=c(-.75,1.5),xlim=c(0,10),xaxt="n", yaxt="n",lwd=wd,bty="n")
lines(x,mg,lty=5,lwd=wd)
points(xdat,ydat,pch=1,cex=.7)
points(x0,m0,pch=15,cex=1)
abline(v=2,lty=3,lwd=wd)
abline(v=4,lty=3,lwd=wd)
abline(v=6,lty=3,lwd=wd)
abline(v=8,lty=3,lwd=wd)
lines(x1,f1,lty=2,lwd=wd)
lines(x2,f2,lty=2,lwd=wd)
lines(x3,f3,lty=2,lwd=wd)
lines(x4,f4,lty=2,lwd=wd)
lines(x5,f5,lty=2,lwd=wd)
axis(side=1,seq(0,10,1),lwd=wd)
axis(side=2,seq(-1,2,.5),lwd=wd)
legend("topright",legend=c("Binned Regression","Rolling Binned","Local Linear"),lty=c(2,1,5),bg="white",lwd=wd,bty="n")
dev.off()
