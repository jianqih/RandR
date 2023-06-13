#############################################
### This file generates Figure 27.3a
### Censored Quantile Regression 
#############################################


L <- 0
U <- 4
a <- 1.5
s <- 2
x <- seq(L,U,.01)
mx <- sqrt(x) - a
sx <- sqrt(s+x)
q1 <- mx + sx*qnorm(.3)
q2 <- mx + sx*qnorm(.5)
q3 <- mx + sx*qnorm(.7)
q4 <- mx + sx*qnorm(.9)
maxy <- max(q1,q2,q3,q4)+0.5
miny <- min(q1,q2,q3,q4)-0.5
q2a <- q2*(q2<0)
q2b <- q2*(q2>0)
q3a <- q3*(q3<0)
q3b <- q3*(q3>0)

wd <- 1.4

pdf("HANSEN27-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,q1,type="l",lty=2,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",xlim=c(-.01,4),ylim=c(miny,maxy),lwd=wd)
title(ylab="Dependent Variable",line=1)
lines(x,q2a,lty=2,lwd=wd)
lines(x,q2b,lwd=wd)
lines(x,q3a,lty=2,lwd=wd)
lines(x,q3b,lwd=wd)
lines(x,q4,lwd=wd)
abline(h=0,lwd=wd)
abline(v=L,lwd=wd)
text(3.7,-0.5,expression(q[.3](x)))
text(3.7,0.8,expression(q[.5](x)))
text(3.7,2,expression(q[.7](x)))
text(3.7,3.9,expression(q[.9](x)))
dev.off()

postscript("HANSEN27-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,q1,type="l",lty=2,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",xlim=c(-.01,4),ylim=c(miny,maxy),lwd=wd)
title(ylab="Dependent Variable",line=1)
lines(x,q2a,lty=2,lwd=wd)
lines(x,q2b,lwd=wd)
lines(x,q3a,lty=2,lwd=wd)
lines(x,q3b,lwd=wd)
lines(x,q4,lwd=wd)
abline(h=0,lwd=wd)
abline(v=L,lwd=wd)
text(3.7,-0.5,expression(q[.3](x)))
text(3.7,0.8,expression(q[.5](x)))
text(3.7,2,expression(q[.7](x)))
text(3.7,3.9,expression(q[.9](x)))
dev.off()

