#####################################
### This file generates 
### Figure 23.1a Box-Cox Transformation
#####################################

BoxCox <- function(x,lambda) if (lambda==0) y <- log(x) else y <- ((x^lambda)-1)/lambda 

x <- seq(0.01,2,.01)
f1 <- BoxCox(x,1)
f2 <- BoxCox(x,.5)
f3 <- BoxCox(x,0)
f4 <- BoxCox(x,-1)
f5 <- BoxCox(x,2)

wd <- 1.4

pdf("HANSEN23-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Box-Cox Transformation",xlab="Variable",yaxt="n",xaxt="n",ylim=c(-1,1),xlim=c(0,2),lwd=wd,bty="n")
axis(side=1,seq(0,2,.4),lwd=wd)
axis(side=2,seq(-1,1,.4),lwd=wd)
lines(x,f2,lty=2,lwd=wd)
lines(x,f3,lty=4,lwd=wd)
lines(x,f4,lty=5,lwd=wd)
lines(x,f5,lty=6,lwd=wd)
legend("topleft",legend=c(expression(lambda==2),expression(lambda==1),expression(lambda==0.5),expression(lambda==0),expression(lambda==-1)), lty=c(6,1,2,4,5),lwd=wd,bty="n")
dev.off()

postscript("HANSEN23-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Box-Cox Transformation",xlab="Variable",yaxt="n",xaxt="n",ylim=c(-1,1),xlim=c(0,2),lwd=wd,bty="n")
axis(side=1,seq(0,2,.4),lwd=wd)
axis(side=2,seq(-1,1,.4),lwd=wd)
lines(x,f2,lty=2,lwd=wd)
lines(x,f3,lty=4,lwd=wd)
lines(x,f4,lty=5,lwd=wd)
lines(x,f5,lty=6,lwd=wd)
legend("topleft",legend=c(expression(lambda==2),expression(lambda==1),expression(lambda==0.5),expression(lambda==0),expression(lambda==-1)), lty=c(6,1,2,4,5),lwd=wd,bty="n")
dev.off()
