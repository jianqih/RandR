#########################################################################
##  This file generates Figure 27.3b
##  Linear Spline of Transfers on Income
#########################################################################
##  Uses figure27_3b.txt
#########################################################################


dat <- read.table("figure27_3b.txt")
qq <- dat[,1]
qn <- length(qq)
f1 <- dat[,2]
f2 <- dat[,3]
f3 <- dat[,4]
f4 <- dat[,5]

wd <- 1.4

pdf("HANSEN27-3b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(qq,f1,type="l",lty=1,xlim=c(0,200000),ylim=c(0,15000),ylab="Transfers (Pesos)",xlab="Total Income (Pesos)",xaxs="i",yaxs="i",yaxt="n",lwd=wd,bty="n")
axis(side=2,seq(0,16000,4000),lwd=wd)
points(qq[2:(qn-1)],f1[2:(qn-1)],pch=0,cex=.9)
lines(qq,f2,lty=1,lwd=wd)
points(qq[2:(qn-1)],f2[2:(qn-1)],pch=15,cex=.9)
lines(qq,f3,lty=1,lwd=wd)
points(qq[2:(qn-1)],f3[2:(qn-1)],pch=17,cex=.9)
lines(qq,f4,lty=1,lwd=wd)
points(qq[2:(qn-1)],f4[2:(qn-1)],pch=19,cex=.9)
text(180000,8300,"OLS")
text(180000,4300,"Tobit")
text(180000,2600,"LAD")
text(180000,1000,"CLAD")
dev.off()

postscript("HANSEN27-3b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(qq,f1,type="l",lty=1,xlim=c(0,200000),ylim=c(0,15000),ylab="Transfers (Pesos)",xlab="Total Income (Pesos)",xaxs="i",yaxs="i",yaxt="n",lwd=wd,bty="n")
axis(side=2,seq(0,16000,4000),lwd=wd)
points(qq[2:(qn-1)],f1[2:(qn-1)],pch=0,cex=.9)
lines(qq,f2,lty=1,lwd=wd)
points(qq[2:(qn-1)],f2[2:(qn-1)],pch=15,cex=.9)
lines(qq,f3,lty=1,lwd=wd)
points(qq[2:(qn-1)],f3[2:(qn-1)],pch=17,cex=.9)
lines(qq,f4,lty=1,lwd=wd)
points(qq[2:(qn-1)],f4[2:(qn-1)],pch=19,cex=.9)
text(180000,8300,"OLS")
text(180000,4300,"Tobit")
text(180000,2600,"LAD")
text(180000,1000,"CLAD")
dev.off()

