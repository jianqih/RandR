########################################
### This file generates Figure 7.2b 
### Density of Normalized OLS estimator
### with error process (7.12)
########################################


r <- 100000
n <- 100
k_vec <- c(8,6,4)
x <- seq(-2.5,2.5,.01)
xn <- length(x)

f <- matrix(0,xn,5)
for (i in 1:3){
  k <- k_vec[i]
  m1 <- prod(seq(from=1,by=2,length.out=k/2))
  m2 <- prod(seq(from=1,by=2,length.out=k))
  y <-  matrix(rnorm(n*r),n,r)^k
  y1 <- (y-m1)/sqrt(m2-m1^2)
  beta <- colMeans(y1)*sqrt(n)
  bd <- .7*sd(beta)/(r^(.2))
  for (j in 1:xn){
    xj <- x[j]
    f[j,i] <- mean(dnorm((beta-xj)/bd))/bd
  }
}
f[,4] <- dnorm(x)

wd = 1.4

leg1 <- expression('N(0,1)'^8)
leg2 <- expression('N(0,1)'^6)
leg3 <- expression('N(0,1)'^4)
leg4 <- expression(N(0,1))
leg <- c(leg1,leg2,leg3,leg4)


pdf("HANSEN7-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f[,1], type="l",lty=5,xlab="",ylab="",ylim=c(0,1.4),xaxs="i",yaxs="i",bty="n",xaxt="n",yaxt="n",lwd=wd)
lines(x,f[,2],lty=6,lwd=wd)
lines(x,f[,3],lty=2,lwd=wd)
lines(x,f[,4],lty=1,lwd=wd)
axis(1,seq(-3,3,1),lwd=wd)
legend("topright",legend=leg,lty=c(5,6,2,1),lwd=wd,bty="n")
dev.off()

postscript("HANSEN7-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f[,1], type="l",lty=5,xlab="",ylab="",ylim=c(0,1.4),xaxs="i",yaxs="i",bty="n",xaxt="n",yaxt="n",lwd=wd)
lines(x,f[,2],lty=6,lwd=wd)
lines(x,f[,3],lty=2,lwd=wd)
lines(x,f[,4],lty=1,lwd=wd)
axis(1,seq(-3,3,1),lwd=wd)
legend("topright",legend=leg,lty=c(5,6,2,1),lwd=wd,bty="n")
dev.off()

