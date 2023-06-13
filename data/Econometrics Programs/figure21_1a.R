#####################################
### This file generates Figure 21.1a
### Regression Discontinuity Design
#####################################

x1 <- as.matrix(seq(0,1,by=0.01))
x2 <- as.matrix(seq(1,2.5,by=0.01))

b1 <- (x1/3)^(1/6)
b2 <- (x2/3)^(1/6)

t1 <- (x1/3)^(1/3)
t2 <- (x2/3)^(1/3)

wd <- 1.4

pdf("HANSEN21-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x1,b1,type="l",lty=1,xaxs="i",yaxs="i",,ylab="Dependent Variable",xlab="Running Variable",bty="n",xlim=c(0,2.5),ylim=c(0,1.1),yaxt="n",xaxt="n",lwd=wd,yaxt="n")
axis(side=1,lwd=wd)
axis(side=2,seq(0,1.2,.2),lwd=wd)
lines(x1,t1,lty=2,lwd=wd)
lines(x2,b2,lty=2,lwd=wd)
lines(x2,t2,lwd=wd)
lines(c(1,1),c(b2[1],t2[1]),lwd=wd)
abline(h=0,lwd=wd)
abline(v=0,lwd=wd)
lines(c(1,1),c(0,.02),lwd=wd)
text(1,.07,"c")
text(1.75,.07,"X>c")
text(0.5,.07,"X<c")
text(.95,0.76,expression(bar(theta)))
text(1.8,.76,expression(m[1](x)))
text(.45,.82,expression(m[0](x)))
dev.off()

postscript("HANSEN21-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x1,b1,type="l",lty=1,xaxs="i",yaxs="i",,ylab="Dependent Variable",xlab="Running Variable",bty="n",xlim=c(0,2.5),ylim=c(0,1.1),yaxt="n",xaxt="n",lwd=wd,yaxt="n")
axis(side=1,lwd=wd)
axis(side=2,seq(0,1.2,.2),lwd=wd)
lines(x1,t1,lty=2,lwd=wd)
lines(x2,b2,lty=2,lwd=wd)
lines(x2,t2,lwd=wd)
lines(c(1,1),c(b2[1],t2[1]),lwd=wd)
abline(h=0,lwd=wd)
abline(v=0,lwd=wd)
lines(c(1,1),c(0,.02),lwd=wd)
text(1,.07,"c")
text(1.75,.07,"X>c")
text(0.5,.07,"X<c")
text(.95,0.76,expression(bar(theta)))
text(1.8,.76,expression(m[1](x)))
text(.45,.82,expression(m[0](x)))
dev.off()
