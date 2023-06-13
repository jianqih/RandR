######################################
### This file generates Figure 9.3b
### Asymptotic Local Power Function 
### Varying q
######################################



alpha <- 0.05
power_f <- function(lambda,q){
  c <- qchisq(1-alpha,q)
  val <- 1-pchisq(c,q,ncp=lambda)
  return(val)
}

# setup grids
nx <- 501
x <- seq(0,10,.02)
q1 <- 1
q2 <- 2
q3 <- 3
c1 <- qchisq(1-alpha,q1)
c2 <- qchisq(1-alpha,q2)
c3 <- qchisq(1-alpha,q3)
f1 <- 1-pchisq(c1,q1,ncp=x)
f2 <- 1-pchisq(c2,q2,ncp=x)
f3 <- 1-pchisq(c3,q3,ncp=x)

b1 <- x[which.min(abs(f1-.5))]
b2 <- x[which.min(abs(f2-.5))]
b3 <- x[which.min(abs(f3-.5))]

wd <- 1.4

pdf("HANSEN9-3b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f1,type="l",xlim=c(0,8),ylim=c(0,1),xlab=expression(lambda),ylab="Power",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(1,seq(0,10,2),lwd=wd)
axis(2,seq(0,1,.2),lwd=wd)
lines(x,f2,lwd=wd)
lines(x,f3,lwd=wd)
lines(c(0,b3),c(.5,.5),lwd=wd,lty=2)
points(c(b1,b2,b3),c(.5,.5,.5),pch=19,cex=.6)
text(4.8,.65,expression(q==1))
text(6,.63,expression(q==2))
text(7.2,.55,expression(q==3))
dev.off()

postscript("HANSEN9-3b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f1,type="l",xlim=c(0,8),ylim=c(0,1),xlab=expression(lambda),ylab="Power",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(1,seq(0,10,2),lwd=wd)
axis(2,seq(0,1,.2),lwd=wd)
lines(x,f2,lwd=wd)
lines(x,f3,lwd=wd)
lines(c(0,b3),c(.5,.5),lwd=wd,lty=2)
points(c(b1,b2,b3),c(.5,.5,.5),pch=19,cex=.6)
text(4.8,.65,expression(q==1))
text(6,.63,expression(q==2))
text(7.2,.55,expression(q==3))

dev.off()
