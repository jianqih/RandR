#####################################
### This file generates Figure 5.1 
### Normal Density and Distribution
#####################################

x <- seq(-4,4,by=0.01)
y <- dnorm(x)
f <- pnorm(x)

# Figure 5.1a

wd <- 1.4

pdf("HANSEN5-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,y,type="l",lty=1,xaxs="i",yaxs="i",xlim=c(-4,4),ylim=c(0,.42),ylab="",xlab="",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-4,4,1),lwd=wd)
axis(side=2,lwd=wd)
text(1.5,.3,expression(phi(x)))
dev.off()

postscript("HANSEN5-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,y,type="l",lty=1,xaxs="i",yaxs="i",xlim=c(-4,4),ylim=c(0,.42),ylab="",xlab="",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-4,4,1),lwd=wd)
text(1.5,.3,expression(phi(x)))
axis(side=2,lwd=wd)
dev.off()


# Figure 5.1b

pdf("HANSEN5-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f,type="l",lty=1,xaxs="i",yaxs="i",xlim=c(-4,4),ylim=c(0,1.02),ylab="",xlab="",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-4,4,1),lwd=wd)
axis(side=2,lwd=wd)
text(1.5,.8,expression(Phi(x)))
dev.off()

postscript("HANSEN5-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f,type="l",lty=1,xaxs="i",yaxs="i",xlim=c(-4,4),ylim=c(0,1.02),ylab="",xlab="",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-4,4,1),lwd=wd)
axis(side=2,lwd=wd)
text(1.5,.8,expression(Phi(x)))
dev.off()



