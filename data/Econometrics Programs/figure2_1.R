#####################################
### This file generates Figure 2.1 
### Wage Distribution and Density 
### of all full-time U.S. workers
#####################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
hrwage <- as.matrix(dat[,5]/(dat[,6]*dat[,7]))
lnwage <- log(hrwage)

den <- density(hrwage,from=0,to=100,adjust=2)	
den1 <- density(lnwage,bw=0.08,from=0,to=6,adjust=2)		

wd <- 1.4	

pdf("HANSEN2-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(den$x, den$y, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,70),ylim=c(0,0.045),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
axis(side=1,seq(0,70,10),lwd=wd)
dev.off()

postscript("HANSEN2-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(den$x, den$y, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,70),ylim=c(0,0.045),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
axis(side=1,seq(0,70,10),lwd=wd)
dev.off()



pdf("HANSEN2-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(den1$x, den1$y, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,6),ylim=c(0,0.7),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
axis(side=1,seq(0,6,1),lwd=wd)
dev.off()

postscript("HANSEN2-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(den1$x, den1$y, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,6),ylim=c(0,0.7),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
axis(side=1,seq(0,6,1),lwd=wd)
dev.off()



