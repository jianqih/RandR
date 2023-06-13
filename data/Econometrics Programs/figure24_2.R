#############################################
### This file generates Figures 24.2-24.3
### Quantile Regression Functions
#############################################
### This file uses package haven
### This file uses data file cps09mar.dta
#############################################

library(haven)

cps09mar <- read_dta("cps09mar.dta")
wage <- cps09mar$earnings/cps09mar$hours/cps09mar$week
edu <- cps09mar$education
lnwage <- log(wage)

edu_dot <- c(4,6,8,9,10,11,12,13,14,16,18,20)
ne <- length(edu_dot)
qq <- c(.1,.3,.5,.7,.9)
nq <- length(qq)

Qreg <- matrix(0,ne,nq)
Qreg2 <- matrix(0,ne,nq)

for (i in 1:ne){
  w <- wage[edu == edu_dot[i]]
  logw <- lnwage[edu == edu_dot[i]]
  for (j in 1:nq) Qreg[i,j] <- quantile(w,qq[j])
  for (j in 1:nq) Qreg2[i,j] <- quantile(logw,qq[j])
}

wd <- 1.4

pdf("HANSEN24-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(edu_dot,Qreg[,1],type="p",cex=.8,pch=19,xlim=c(4,20),ylim=c(5,110),
     xlab="Education (Years)",ylab="Dollars per Hour",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(2,20,2),lwd=wd)
axis(side=2,seq(0,120,20),lwd=wd)
points(edu_dot,Qreg[,2],cex=.8, pch=17)
points(edu_dot,Qreg[,3],cex=.8, pch=21)
points(edu_dot,Qreg[,4],cex=.8, pch=15)
points(edu_dot,Qreg[,5],cex=1.1, pch=18)
for (i in 1:nq) lines(edu_dot,Qreg[,i],lwd=wd)
legend("topleft",c("90%","70%","50%","30%","10%"),pch=c(18,15,21,17,19),pt.cex=c(1.1,.8,.8,.8,.8),lwd=wd,bty="n")
dev.off()

postscript("HANSEN24-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(edu_dot,Qreg[,1],type="p",cex=.8,pch=19,xlim=c(4,20),ylim=c(5,110),
     xlab="Education (Years)",ylab="Dollars per Hour",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(2,20,2),lwd=wd)
axis(side=2,seq(0,120,20),lwd=wd)
points(edu_dot,Qreg[,2],cex=.8, pch=17)
points(edu_dot,Qreg[,3],cex=.8, pch=21)
points(edu_dot,Qreg[,4],cex=.8, pch=15)
points(edu_dot,Qreg[,5],cex=1.1, pch=18)
for (i in 1:nq) lines(edu_dot,Qreg[,i],lwd=wd)
legend("topleft",c("90%","70%","50%","30%","10%"),pch=c(18,15,21,17,19),pt.cex=c(1.1,.8,.8,.8,.8),lwd=wd,bty="n")
dev.off()


pdf("HANSEN24-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(edu_dot,Qreg2[,1],type="p",cex=.8,pch=19,xlim=c(4,20),ylim=c(1.7,4.7),
     xlab="Education (Years)",ylab="Log Dollars per Hour",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(2,20,2),lwd=wd)
axis(side=2,seq(0,6,1),lwd=wd)
points(edu_dot,Qreg2[,2],cex=.8, pch=17)
points(edu_dot,Qreg2[,3],cex=.8, pch=21)
points(edu_dot,Qreg2[,4],cex=.8, pch=15)
points(edu_dot,Qreg2[,5],cex=1.1, pch=18)
for (i in 1:nq) lines(edu_dot,Qreg2[,i],lwd=wd)
legend("topleft",c("90%","70%","50%","30%","10%"),pch=c(18,15,21,17,19),pt.cex=c(1.1,.8,.8,.8,.8),lwd=wd,bty="n")
dev.off()

postscript("HANSEN24-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(edu_dot,Qreg2[,1],type="p",cex=.8,pch=19,xlim=c(4,20),ylim=c(1.7,4.7),
     xlab="Education (Years)",ylab="Log Dollars per Hour",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(2,20,2),lwd=wd)
axis(side=2,seq(0,6,1),lwd=wd)
points(edu_dot,Qreg2[,2],cex=.8, pch=17)
points(edu_dot,Qreg2[,3],cex=.8, pch=21)
points(edu_dot,Qreg2[,4],cex=.8, pch=15)
points(edu_dot,Qreg2[,5],cex=1.1, pch=18)
for (i in 1:nq) lines(edu_dot,Qreg2[,i],lwd=wd)
legend("topleft",c("90%","70%","50%","30%","10%"),pch=c(18,15,21,17,19),pt.cex=c(1.1,.8,.8,.8,.8),lwd=wd,bty="n")
dev.off()

x <- seq(-2,2,.01)
m <- x*(.5-(x<0))
q <- x*(.2-(x<0))

pdf("HANSEN24-3.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,m,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",cex.lab=.75,xlim=c(-2,2),ylim=c(-.2,2))
lines(x,q,lty=5)
abline(h=0)
text(-1.6,0.6,expression(rho[0.5](x)),cex=.8)
text(1.7,0.25,expression(rho[0.2](x)),cex=.8)
text(1.7,-.1,"X",cex=.8)
dev.off()

postscript("HANSEN24-3.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,m,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xaxt="n",yaxt="n",xlab="",bty="n",cex.lab=.75,xlim=c(-2,2),ylim=c(-.2,2))
lines(x,q,lty=5)
abline(h=0)
text(-1.6,0.6,expression(rho[0.5](x)),cex=.8)
text(1.7,0.25,expression(rho[0.2](x)),cex=.8)
text(1.7,-.1,"X",cex=.8)
dev.off()

