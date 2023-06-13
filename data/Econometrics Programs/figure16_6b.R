#####################################
### This file generates Figure 16.6b
### Spurious Regression T Coverage
#####################################
### Uses file figure16_6b.txt
#########################################

dat <- read.table("figure16_6b.txt")
ns <- dat[,1]
coverage <- dat[,2]

wd <- 1.4

pdf("HANSEN16-6b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(ns,coverage,type="l",xaxs="i",yaxs="i",ylab="Probability",xlab="Sample Size",bty="n",xaxt="n",yaxt="n",xlim=c(0,200),ylim=c(0,.7),lwd=wd) 
axis(side=1,seq(0,300,50),lwd=wd)
axis(side=2,seq(0,1,.2),lwd=wd)
dev.off()

postscript("HANSEN16-6b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(ns,coverage,type="l",xaxs="i",yaxs="i",ylab="Probability",xlab="Sample Size",bty="n",xaxt="n",yaxt="n",xlim=c(0,200),ylim=c(0,.7),lwd=wd) 
axis(side=1,seq(0,300,50),lwd=wd)
axis(side=2,seq(0,1,.2),lwd=wd)
dev.off()
