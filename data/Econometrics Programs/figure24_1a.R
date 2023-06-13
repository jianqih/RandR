#####################################
### This file generates Figure 24.1a
### LAD Criterion
#####################################


x <- seq(-2,2,.01)
m <- abs(x)
q <- x^2

wd <- 1.4

pdf("HANSEN24-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,q,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",ylim=c(-.3,4),xlim=c(-2,2),lwd=wd,bty="n")
lines(x,m,lty=5,lwd=wd)
abline(h=0,lwd=wd)
text(1.5,1.2,expression(abs(x)))
text(1.5,3,expression(x^2))
text(1.7,-.2,"X",cex=.8)
dev.off()


postscript("HANSEN24-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,q,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",ylim=c(-.3,4),xlim=c(-2,2),lwd=wd,bty="n")
lines(x,m,lty=5,lwd=wd)
abline(h=0,lwd=wd)
text(1.5,1.2,expression(abs(x)))
text(1.5,3,expression(x^2))
text(1.7,-.2,"X",cex=.8)
dev.off()
