#####################################
### This file generates Figure 21.3
### Regression Discontinuity Design
#####################################


# Figure 21.3a
c <- 1
a <- 1.5
b <- .5
x1 <- as.matrix(seq(0,c,by=0.01))
x2 <- as.matrix(seq(c,2.5,by=0.01))
x <- rbind(x1,x2)
f1 <- pnorm(x1-a)
f2 <- pnorm(x2-b)

wd <- 1.4

pdf("HANSEN21-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x1,f1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Probability of Treatment",xlab="Running Variable",xlim=c(0,2.5),ylim=c(0,1),xaxt="n",lwd=wd,bty="n") 
axis(side=1,lwd=wd)
axis(side=2,lwd=wd)
lines(x2,f2,lwd=wd)
lines(c(c,c),c(pnorm(c-a),pnorm(c-b)),lwd=wd)
lines(c(1,1),c(0,.02),lwd=wd)
text(1,.07,"c")
text(1.6,.8,expression(p(x)))
dev.off()

postscript("HANSEN21-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x1,f1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Probability of Treatment",xlab="Running Variable",xlim=c(0,2.5),ylim=c(0,1),xaxt="n",lwd=wd,bty="n") 
axis(side=1,lwd=wd)
axis(side=2,lwd=wd)
lines(x2,f2,lwd=wd)
lines(c(c,c),c(pnorm(c-a),pnorm(c-b)),lwd=wd)
lines(c(1,1),c(0,.02),lwd=wd)
text(1,.07,"c")
text(1.6,.8,expression(p(x)))
dev.off()


# Figure 21.3b

m0 <- (x/3)^(1/6)
m1 <- (x/3)^.75
f <- rbind(f1,f2)
m <- f*m1 + (1-f)*m0

pdf("HANSEN21-3b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,m0,type="l",lty=5,xaxs="i",yaxs="i",ylab="Dependent Variable",xlab="Running Variable",bty="n",xlim=c(0,2.5),ylim=c(0,1.1),yaxt="n",xaxt="n",lwd=wd,bty="n")
axis(side=1,lwd=wd)
axis(side=2,seq(0,1.2,.2),lwd=wd)
lines(x,m1,lty=5,lwd=wd)
lines(x,m,lwd=wd)
abline(h=0,lwd=wd)
abline(v=0,lwd=wd)
lines(c(1,1),c(0,.02),lwd=wd)
text(1,.07,"c")
text(1.75,.07,"X>c")
text(0.5,.07,"X<c")
text(1.8,.58,expression(m[1](x)))
text(.45,.8,expression(m[0](x)))
text(1.5,.7,expression(m(x)))
dev.off()

postscript("HANSEN21-3b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,m0,type="l",lty=5,xaxs="i",yaxs="i",ylab="Dependent Variable",xlab="Running Variable",bty="n",xlim=c(0,2.5),ylim=c(0,1.1),yaxt="n",xaxt="n",lwd=wd,bty="n")
axis(side=1,lwd=wd)
axis(side=2,seq(0,1.2,.2),lwd=wd)
lines(x,m1,lty=5,lwd=wd)
lines(x,m,lwd=wd)
abline(h=0,lwd=wd)
abline(v=0,lwd=wd)
lines(c(1,1),c(0,.02),lwd=wd)
text(1,.07,"c")
text(1.75,.07,"X>c")
text(0.5,.07,"X<c")
text(1.8,.58,expression(m[1](x)))
text(.45,.8,expression(m[0](x)))
text(1.5,.7,expression(m(x)))
dev.off()
