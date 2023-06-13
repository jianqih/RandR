#########################################################################
##  This file generates Figure 15.1
##  Impulse Response Functions
#########################################################################
### Uses files figure15_1a.txt, figure15_1b.txt
#########################################

dat <- read.table("figure15_1a.txt")
x <- dat[,1]
f <- dat[,2]
fL <- dat[,3]
fU <- dat[,4]

wd <- 1.4

pdf("HANSEN15-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f,type="l",xaxs="i",yaxs="i",ylab="",xlab="Quarters",bty="n",xaxt="n",yaxt="n",ylim=c(-.3,.1),xlim=c(0,20),lwd=wd) 
polygon(c(x,rev(x)),c(fL,rev(fU)),border=NA,col=grey(.8),lwd=wd)
axis(side=1,seq(0,20,4),lwd=wd)
axis(side=2,seq(-.4,.2,.1),lwd=wd)
abline(h=0,lwd=wd,lty=2)
lines(x,f,lwd=wd)
dev.off()

postscript("HANSEN15-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f,type="l",xaxs="i",yaxs="i",ylab="",xlab="Quarters",bty="n",xaxt="n",yaxt="n",ylim=c(-.3,.1),xlim=c(0,20),lwd=wd) 
polygon(c(x,rev(x)),c(fL,rev(fU)),border=NA,col=grey(.8),lwd=wd)
axis(side=1,seq(0,20,4),lwd=wd)
axis(side=2,seq(-.4,.2,.1),lwd=wd)
abline(h=0,lwd=wd,lty=2)
lines(x,f,lwd=wd)
dev.off()

dat <- read.table("figure15_1b.txt")
x <- dat[,1]
f <- dat[,2]
fL <- dat[,3]
fU <- dat[,4]

wd <- 1.4

pdf("HANSEN15-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f,type="l",xaxs="i",yaxs="i",ylab="",xlab="Quarters",bty="n",yaxt="n",xaxt="n",ylim=c(-1.05,.1),xlim=c(0,20),lwd=wd) 
polygon(c(x,rev(x)),c(fL,rev(fU)),border=NA,col=grey(.8),lwd=wd)
axis(side=1,seq(0,20,4),lwd=wd)
axis(side=2,seq(-2,1,.4),lwd=wd)
abline(h=0,lwd=wd,lty=2)
lines(x,f,lwd=wd)
dev.off()

postscript("HANSEN15-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f,type="l",xaxs="i",yaxs="i",ylab="",xlab="Quarters",bty="n",yaxt="n",xaxt="n",ylim=c(-1.05,.1),xlim=c(0,20),lwd=wd) 
polygon(c(x,rev(x)),c(fL,rev(fU)),border=NA,col=grey(.8),lwd=wd)
axis(side=1,seq(0,20,4),lwd=wd)
axis(side=2,seq(-2,1,.4),lwd=wd)
abline(h=0,lwd=wd,lty=2)
lines(x,f,lwd=wd)
dev.off()

