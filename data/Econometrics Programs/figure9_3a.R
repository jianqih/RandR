######################################
### This file generates Figure 9.3a
### Asymptotic Local Power Function 
### of One-Sided t Test
######################################

alpha1 <- 0.05
alpha2 <- 0.01
c1 <- qnorm(1-alpha1)
c2 <- qnorm(1-alpha2)
nx <- 501
x <- seq(-1,4,0.01)
f1 <- pnorm(x-c1)
f2 <- pnorm(x-c2)
a1 <- pnorm(1-c1)
a2 <- pnorm(1-c2)

wd <- 1.4

pdf("HANSEN9-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f1,type="l",xlim=c(-1,4),ylim=c(0,1),xlab=expression(delta),ylab="Power",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(1,seq(-1,4,1),lwd=wd)
axis(2,seq(0,1,.2),lwd=wd)
lines(x,f2,lwd=wd)
lines(c(1,1),c(0,a1),lwd=wd,lty=2)
points(c(1,1),c(a1,a2),pch=19,cex=.6)
lines(c(-1,c2),c(.5,.5),lwd=wd,lty=2)
points(c(c1,c2),c(.5,.5),pch=19,cex=.6)
text(2,.85,expression(alpha==0.05))
text(3.4,.7,expression(alpha==0.10))
dev.off()

postscript("HANSEN9-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f1,type="l",xlim=c(-1,4),ylim=c(0,1),xlab=expression(delta),ylab="Power",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(1,seq(-1,4,1),lwd=wd)
axis(2,seq(0,1,.2),lwd=wd)
lines(x,f2,lwd=wd)
lines(c(1,1),c(0,a1),lwd=wd,lty=2)
points(c(1,1),c(a1,a2),pch=19,cex=.6)
lines(c(-1,c2),c(.5,.5),lwd=wd,lty=2)
points(c(c1,c2),c(.5,.5),pch=19,cex=.6)
text(2,.85,expression(alpha==0.05))
text(3.4,.7,expression(alpha==0.10))
dev.off()