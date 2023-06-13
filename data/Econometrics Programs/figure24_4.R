#############################################
### This file generates Figure 24.4
### Quantile Regression Examples
#############################################

x <- c(0,10)
q1 <- c(.1,.5)
q2 <- c(.2,1)
q3 <- c(.3,1.5)
q4 <- c(.4,2)
q5 <- c(.5,2.5)

lwd <- 1.4

pdf("HANSEN24-4a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,q1,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",xlim=c(-.5,10),ylim=c(-.25,2.5),lwd=wd)
lines(x,q2,lwd=wd)
lines(x,q3,lwd=wd)
lines(x,q4,lwd=wd)
lines(x,q5,lwd=wd)
lines(c(0,10),c(0,0),lwd=wd)
lines(c(0,0),c(0,2.5),lwd=wd)
text(9,0.35,expression(q[.1](x)))
text(9,0.8,expression(q[.3](x)))
text(9,1.25,expression(q[.5](x)))
text(9,1.65,expression(q[.7](x)))
text(9,2.1,expression(q[.9](x)))
text(9,-.15,"X")
text(-.3,2.3,"Y")
dev.off()

postscript("HANSEN24-4a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,q1,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",xlim=c(-.5,10),ylim=c(-.25,2.5),lwd=wd)
lines(x,q2,lwd=wd)
lines(x,q3,lwd=wd)
lines(x,q4,lwd=wd)
lines(x,q5,lwd=wd)
lines(c(0,10),c(0,0),lwd=wd)
lines(c(0,0),c(0,2.5),lwd=wd)
text(9,0.35,expression(q[.1](x)))
text(9,0.8,expression(q[.3](x)))
text(9,1.25,expression(q[.5](x)))
text(9,1.65,expression(q[.7](x)))
text(9,2.1,expression(q[.9](x)))
text(9,-.15,"X")
text(-.3,2.3,"Y")
dev.off()


x <- seq(0,10,.01)
sx <- .12*x + sin(x*2*pi/10)*.3
q1 <- .1 + sx
q2 <- .3 + sx
q3 <- .5 + sx
q4 <- .7 + sx
q5 <- .9 + sx

pdf("HANSEN24-4b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,q1,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",xlim=c(-.5,10),ylim=c(-.25,2.1),lwd=wd)
lines(x,q2,lwd=wd)
lines(x,q3,lwd=wd)
lines(x,q4,lwd=wd)
lines(x,q5,lwd=wd)
lines(c(0,10),c(0,0),lwd=wd)
lines(c(0,0),c(0,2.1),lwd=wd)
text(1.25,0.2,expression(q[.1](x)))
text(3.6,1.03,expression(q[.3](x)))
text(6.2,1.125,expression(q[.5](x)))
text(6.7,1.325,expression(q[.7](x)))
text(8.8,2.0,expression(q[.9](x)))
text(9,-.15,"X")
text(-.3,1.9,"Y")
dev.off()

postscript("HANSEN24-4b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,q1,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",xlim=c(-.5,10),ylim=c(-.25,2.1),lwd=wd)
lines(x,q2,lwd=wd)
lines(x,q3,lwd=wd)
lines(x,q4,lwd=wd)
lines(x,q5,lwd=wd)
lines(c(0,10),c(0,0),lwd=wd)
lines(c(0,0),c(0,2.1),lwd=wd)
text(1.25,0.2,expression(q[.1](x)))
text(3.6,1.03,expression(q[.3](x)))
text(6.2,1.125,expression(q[.5](x)))
text(6.7,1.325,expression(q[.7](x)))
text(8.8,2.0,expression(q[.9](x)))
text(9,-.15,"X")
text(-.3,1.9,"Y")
dev.off()

