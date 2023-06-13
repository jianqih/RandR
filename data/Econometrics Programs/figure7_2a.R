########################################
### This file generates Figure 7.2a
### Density of Normalized OLS estimator
### with Double Pareto Error
########################################


r <- 100000
n <- 100
x0 <- matrix(rnorm(n*r),n,r)
x <- scale(x0,scale=FALSE)
u <- matrix(runif(n*r),n,r)
s <- (matrix(runif(n*r),n,r)<.5)*2-1

alpha_vec <- c(2.4,2.2,2.1,2.05)
an <- length(alpha_vec)

for(i in 1:an){
  alpha <- alpha_vec[i]
  y <- (u^(-1/alpha))*s/sqrt(alpha/(alpha-2))
  beta <- colSums(x*y)/colSums(x^2)*sqrt(n)
  var1 <- paste("f",i,sep="")
  bd <- 1.5*bw.nrd(beta)
  assign(var1, density(beta,n=201,from=-2.5,to=2.5,bw=bd))
}

p <- dnorm(f1$x)

wd <- 1.4

leg1 <- expression(alpha==2.05)
leg2 <- expression(alpha==2.1)
leg3 <- expression(alpha==2.2)
leg4 <- expression(alpha==2.4)
leg5 <- expression(N(0,1))
leg <- c(leg1,leg2,leg3,leg4,leg5)

pdf("HANSEN7-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(f4$x,f4$y, type="l",lty=5,xlab="",ylab="",ylim=c(0,1),xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",lwd=wd)
lines(f1$x,p,lty=1,lwd=wd)
lines(f1$x,f1$y,lty=6,lwd=wd)
lines(f2$x,f2$y,lty=2,lwd=wd)
lines(f3$x,f3$y,lty=4,lwd=wd)
legend("topright",legend=leg,lty=c(5,4,2,6,1),lwd=wd,bty="n")
axis(1,seq(-3,3,1),lwd=wd)
dev.off()

postscript("HANSEN7-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(f4$x,f4$y, type="l",lty=5,xlab="",ylab="",ylim=c(0,1),xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",lwd=wd)
lines(f1$x,p,lty=1,lwd=wd)
lines(f1$x,f1$y,lty=6,lwd=wd)
lines(f2$x,f2$y,lty=2,lwd=wd)
lines(f3$x,f3$y,lty=4,lwd=wd)
legend("topright",legend=leg,lty=c(5,4,2,6,1),lwd=wd,bty="n")
axis(1,seq(-3,3,1),lwd=wd)
dev.off()

