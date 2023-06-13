#############################################
### This file generates Figure 24.5
### Quantile Crossings
#############################################
### This file uses packages haven, quantreg
### This file uses data file cps09mar.dta
#############################################

library(haven)
library(quantreg)

cps09mar <- read_dta("cps09mar.dta")
wage <- cps09mar$earnings/cps09mar$hours/cps09mar$week
edu <- cps09mar$education
lnwage <- log(wage)
edu1 <- (edu>11)*(edu-11)

qq <- c(.1,.3,.5,.7,.9)
nq <- length(qq)
x <- seq(0,20)
nx <- length(x)

Qreg1 <- matrix(0,nx,nq)
Qreg2 <- matrix(0,nx,nq)
Qreg3 <- matrix(0,nx,nq)

for (i in 1:nq){
  q1 <- rq(wage~edu,tau=qq[i])
  b1 <- q1$coef
  Qreg1[,i] <- b1[1] + x*b1[2]

  q2 <- rq(lnwage~edu,tau=qq[i])
  b2 <- q2$coef
  Qreg2[,i] <- exp(b2[1] + x*b2[2])

  q3 <- rq(wage~edu+edu1,tau=qq[i])
  b3 <- q3$coef
  Qreg3[,i] <- b3[1] + x*b3[2] + (x-11)*(x>11)*b3[3]

}

wd <- 1.4

pdf("HANSEN24-5a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,Qreg1[,1],type="l",cex=1.1,pch=19,xlim=c(0,20),ylim=c(-10,70),xlab="Education (Years)",ylab="Wage",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-4,20,4),lwd=wd)
axis(side=2,seq(-20,120,20),lwd=wd)
for (i in 2:nq) lines(x,Qreg1[,i],lwd=wd)
text(19,12,expression(q[.1]))
text(19,20,expression(q[.3]))
text(19,27,expression(q[.5]))
text(19,35,expression(q[.7]))
text(19,52,expression(q[.9]))
dev.off()

postscript("HANSEN24-5a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,Qreg1[,1],type="l",cex=1.1,pch=19,xlim=c(0,20),ylim=c(-10,70),xlab="Education (Years)",ylab="Wage",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-4,20,4),lwd=wd)
axis(side=2,seq(-20,120,20),lwd=wd)
for (i in 2:nq) lines(x,Qreg1[,i],lwd=wd)
text(19,12,expression(q[.1]))
text(19,20,expression(q[.3]))
text(19,27,expression(q[.5]))
text(19,35,expression(q[.7]))
text(19,52,expression(q[.9]))
dev.off()


pdf("HANSEN24-5b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,Qreg2[,1],type="l",cex=1.1,pch=19,xlim=c(0,20),ylim=c(-10,70),xlab="Education (Years)",ylab="Wage",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-4,20,4),lwd=wd)
axis(side=2,seq(-20,120,20),lwd=wd)
for (i in 2:nq) lines(x,Qreg2[,i],lwd=wd)
text(19,13,expression(q[.1]))
text(19,21,expression(q[.3]))
text(19,29,expression(q[.5]))
text(19,38,expression(q[.7]))
text(19,58,expression(q[.9]))
dev.off()

postscript("HANSEN24-5b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,Qreg2[,1],type="l",cex=1.1,pch=19,xlim=c(0,20),ylim=c(-10,70),xlab="Education (Years)",ylab="Wage",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-4,20,4),lwd=wd)
axis(side=2,seq(-20,120,20),lwd=wd)
for (i in 2:nq) lines(x,Qreg2[,i],lwd=wd)
text(19,13,expression(q[.1]))
text(19,21,expression(q[.3]))
text(19,29,expression(q[.5]))
text(19,38,expression(q[.7]))
text(19,58,expression(q[.9]))
dev.off()


