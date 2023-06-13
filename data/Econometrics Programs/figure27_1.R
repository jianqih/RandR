#########################################################################
##  This file generates Figure 27.1
##  Censored Distributions
#########################################################################
##  Uses packages haven, diagram
##  Uses data file CHJ2004.dta
#########################################################################

library(haven)
library(diagram)

dat <- read_dta("CHJ2004.dta")
tr <- as.matrix(dat$tabroad)
t0 <- (tr==0)
cat("Percentage zero\n")
print(mean(t0))
cat("Percentage above 20000\n")
print(mean(tr>20000))

t1 <- tr[(tr>0)]
den <- density(t1,from=0,to=100000,adjust=.3)

wd <- 1.4

pdf("HANSEN27-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(den$x,den$y,type="l",lty=1,xlab="Transfers (Pesos)",ylab="",xaxt="n", yaxt="n",ylim=c(0,.000205),xlim=c(-500,20000),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
axis(side=1,seq(-5000,20000,5000),lwd=wd)
text(6000,.000015,"20%")
polygon(c(-350,-350,350,350),c(0,.0002,.0002,0),col=adjustcolor("white",1),lwd=wd)
text(2000,.00019,"80%")
text(18000,.00003,expression(f(y)))
dev.off()

postscript("HANSEN27-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(den$x,den$y,type="l",lty=1,xlab="Transfers (Pesos)",ylab="",xaxt="n", yaxt="n",ylim=c(0,.000205),xlim=c(-500,20000),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
axis(side=1,seq(-5000,20000,5000),lwd=wd)
text(6000,.000015,"20%")
polygon(c(-350,-350,350,350),c(0,.0002,.0002,0),col=adjustcolor("white",1),lwd=wd)
text(2000,.00019,"80%")
text(18000,.00003,expression(f(y)))
dev.off()

mu <- 1
x0 <- seq(-2,0,.1)
x1 <- seq(0,4,.1)
f0 <- dnorm(x0-mu)
f1 <- dnorm(x1-mu)

pdf("HANSEN27-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
par(xpd=NA)
plot(x1,f1,type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n",ylim=c(0,.65),xlim=c(-2,4),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
abline(h=0,lwd=wd)
lines(x0,f0,lty=5,lwd=wd)
lines(c(mu,mu),c(0,.4),lty=3,lwd=wd)
polygon(c(-.05,-.05,.05,.05),c(0,.6,.6,0),col=adjustcolor("white",1),lwd=wd)
curvedarrow((c(-1,.03)),(c(0,.62)),lty=1,arr.pos=1,curve=-.15,arr.type="simple",angle=50,lwd=wd)
text(0,-.04,"0")
text(mu,-.04,expression(paste("X'",beta)))
text(3.5,-.04,expression(Y^'*'))
text(3.2,.12,expression(f(y)))
text(.8,.55,"P[Y*=0]")
dev.off()


postscript("HANSEN27-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
par(xpd=NA)
plot(x1,f1,type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n",ylim=c(0,.65),xlim=c(-2,4),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
abline(h=0,lwd=wd)
lines(x0,f0,lty=5,lwd=wd)
lines(c(mu,mu),c(0,.4),lty=3,lwd=wd)
polygon(c(-.05,-.05,.05,.05),c(0,.6,.6,0),col=adjustcolor("white",1),lwd=wd)
curvedarrow((c(-1,.03)),(c(0,.62)),lty=1,arr.pos=1,curve=-.15,arr.type="simple",angle=50,lwd=wd)
text(0,-.04,"0")
text(mu,-.04,expression(paste("X'",beta)))
text(3.5,-.04,expression(Y^'*'))
text(3.2,.12,expression(f(y)))
text(.8,.55,"P[Y*=0]")
dev.off()
