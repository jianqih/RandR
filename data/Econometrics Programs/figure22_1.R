#####################################
### This file generates Figures 22.1-22.2
### Uniform Convergence, Consistency
#####################################


x <- seq(.2,1,.001)
b <- -dbeta(x,2,2)
b1 <- b - dbeta(x,15,2)/4
b2 <- b - dbeta(x,30,2)/6.5
b3 <- b - dbeta(x,70,2)/13.5
t1 <- x[which.min(b1)]
t2 <- x[which.min(b2)]
t3 <- x[which.min(b3)]

leg1 <- expression(S[n[1]](theta))
leg2 <- expression(S[n[2]](theta))
leg3 <- expression(S[n[3]](theta))
legb <- expression(S(theta))

wd <- 1.4

pdf("HANSEN22-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,b3,type="l",lty=6,xaxs="i",yaxs="i",xlim=c(.3,1.005),ylim=c(-2.52,0),ylab="",xlab="",xaxt="n",yaxt="n",bty="n",lwd=wd)
abline(v=1,lwd=wd)
abline(h=-2.5,lwd=wd)
lines(x,b3,lty=4,lwd=wd)
lines(x,b2,lty=5,lwd=wd)
lines(x,b1,lty=2,lwd=wd)
lines(x,b,lty=1,lwd=wd)
legend("topleft",legend=c(legb,leg1,leg2,leg3),lty=c(1,2,5,6),y.intersp=1.3,lwd=wd,bty="n")
arrows(.5,-dbeta(.5,2,2),.5,-2.5,angle=20,length=.1,lwd=wd)
arrows(t1,min(b1),t1,-2.5,angle=20,length=.1,lwd=wd)
arrows(t2,min(b2),t2,-2.5,angle=20,length=.1,lwd=wd)
arrows(t3,min(b3),t3,-2.5,angle=20,length=.1,lwd=wd)
par(xpd=NA)
text(.5,-2.7,expression(theta[0]))
text(t1+.005,-2.7,expression(hat(theta)[n[1]]))
text(t2+.005,-2.7,expression(hat(theta)[n[2]]))
text(t3+.02,-2.7,expression(hat(theta)[n[3]]))
dev.off()

postscript("HANSEN22-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,b3,type="l",lty=6,xaxs="i",yaxs="i",xlim=c(.3,1.005),ylim=c(-2.52,0),ylab="",xlab="",xaxt="n",yaxt="n",bty="n",lwd=wd)
abline(v=1,lwd=wd)
abline(h=-2.5,lwd=wd)
lines(x,b3,lty=4,lwd=wd)
lines(x,b2,lty=5,lwd=wd)
lines(x,b1,lty=2,lwd=wd)
lines(x,b,lty=1,lwd=wd)
legend("topleft",legend=c(legb,leg1,leg2,leg3),lty=c(1,2,5,6),y.intersp=1.3,lwd=wd)
arrows(.5,-dbeta(.5,2,2),.5,-2.5,angle=20,length=.1,lwd=wd)
arrows(t1,min(b1),t1,-2.5,angle=20,length=.1,lwd=wd)
arrows(t2,min(b2),t2,-2.5,angle=20,length=.1,lwd=wd)
arrows(t3,min(b3),t3,-2.5,angle=20,length=.1,lwd=wd)
par(xpd=NA)
text(.5,-2.7,expression(theta[0]))
text(t1+.005,-2.7,expression(hat(theta)[n[1]]))
text(t2+.005,-2.7,expression(hat(theta)[n[2]]))
text(t3+.02,-2.7,expression(hat(theta)[n[3]]))
dev.off()



x <- seq(-1,1,by=0.01)
S <- (x^2)
ep <- .15
S1 <- S + ep
S2 <- S - ep
Sn <- S + sin(x*10)*ep*.75

pdf("HANSEN22-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,S,type="l",lty=1,xaxs="i",yaxs="i",xlab="",ylab="",xlim=c(-1,1),ylim=c(-.35,1.3),lwd=2*wd,xaxt="n",yaxt="n",bty="n")
lines(x,S1,lty=5,lwd=wd)
lines(x,S2,lty=5,lwd=wd)
lines(x,Sn,lwd=wd)
leg1 <- expression(S(theta)+epsilon)
leg2 <- expression(S(theta)-epsilon)
leg3 <- expression(S(theta))
leg4 <- expression(S[n](theta))
text(-.6,1,leg1)
text(-.8,0,leg2)
text(.25,.4,leg3)
text(-.2,-.3,leg4)
arrows(-.8,.05,-.7,.32,angle=20,length=.1,lwd=wd)
arrows(-.6,.95,-.65,.6,angle=20,length=.1,lwd=wd)
arrows(.25,.35,.4,.2,angle=20,length=.1,lwd=wd)
arrows(-.2,-.25,-.15,-.1,angle=20,length=.1,lwd=wd)
dev.off()

postscript("HANSEN22-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,S,type="l",lty=1,xaxs="i",yaxs="i",xlab="",ylab="",xlim=c(-1,1),ylim=c(-.35,1.3),lwd=2*wd,xaxt="n",yaxt="n",bty="n")
lines(x,S1,lty=5,lwd=wd)
lines(x,S2,lty=5,lwd=wd)
lines(x,Sn,lwd=wd)
leg1 <- expression(S(theta)+epsilon)
leg2 <- expression(S(theta)-epsilon)
leg3 <- expression(S(theta))
leg4 <- expression(S[n](theta))
text(-.6,1,leg1)
text(-.8,0,leg2)
text(.25,.4,leg3)
text(-.2,-.3,leg4)
arrows(-.8,.05,-.7,.32,angle=20,length=.1,lwd=wd)
arrows(-.6,.95,-.65,.6,angle=20,length=.1,lwd=wd)
arrows(.25,.35,.4,.2,angle=20,length=.1,lwd=wd)
arrows(-.2,-.25,-.15,-.1,angle=20,length=.1,lwd=wd)
dev.off()


x <- seq(-.75,1.25,by=0.01)
f <- (x^2)
fd <- .025
l0 <- (.5)^2 + fd
l1 <- (.5)^2

x1 <- seq(-.75,0,by=0.01)
x2 <- seq(0,.5,by=0.01)
x3 <- seq(.5,1.25,by=0.01)
f1 <- (x1-.5)^2 + fd
f2 <- (x2-.5)^2 + fd
f3 <- (x3-.5)^2 + fd

pdf("HANSEN22-2.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f,type="l",lty=1,xaxs="i",yaxs="i",xlab="",ylab="",xaxt="n",yaxt="n",bty="n",ylim=c(-.1,.56))
lines(x1,f1,lty=5)
lines(x2,f2,lwd=2,lty=5)
lines(x3,f3,lty=5)
leg1 <- expression(S(theta[0]))
leg2 <- expression(S[n](theta[0]))
leg3 <- expression(S[n](hat(theta)))
leg4 <- expression(S(hat(theta)))
points(0,0,pch=19,col="black",cex=.7)
points(.5,fd,pch=19,col="black",cex=.7)
points(0,l0,pch=19,col="black",cex=.7)
points(.5,l1,pch=19,col="black",cex=.7)
text(0,-0.05,leg1,cex=.75)
text(.15,l0,leg2,cex=.75)
text(.5,-.035+fd,leg3,cex=.75)
text(.625,.25,leg4,cex=.75)
lines(c(0,0),c(0,l0),lty=3)
lines(c(.5,.5),c(fd,l1),lty=3)
dev.off()

postscript("HANSEN22-2.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f,type="l",lty=1,xaxs="i",yaxs="i",xlab="",ylab="",xaxt="n",yaxt="n",bty="n",ylim=c(-.1,.56))
lines(x1,f1,lty=5)
lines(x2,f2,lwd=2,lty=5)
lines(x3,f3,lty=5)
leg1 <- expression(S(theta[0]))
leg2 <- expression(S[n](theta[0]))
leg3 <- expression(S[n](hat(theta)))
leg4 <- expression(S(hat(theta)))
points(0,0,pch=19,col="black",cex=.7)
points(.5,fd,pch=19,col="black",cex=.7)
points(0,l0,pch=19,col="black",cex=.7)
points(.5,l1,pch=19,col="black",cex=.7)
text(0,-0.05,leg1,cex=.75)
text(.15,l0,leg2,cex=.75)
text(.5,-.035+fd,leg3,cex=.75)
text(.625,.25,leg4,cex=.75)
lines(c(0,0),c(0,l0),lty=3)
lines(c(.5,.5),c(fd,l1),lty=3)
dev.off()
