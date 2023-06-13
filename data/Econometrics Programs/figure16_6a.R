#####################################
### This file generates Figure 16.6a
### Spurious Regression Correlation Distributions
#####################################
### Uses file figure16_6a.txt
#########################################


dat <- read.table("figure16_6a.txt")
x <- dat[,1]
f1 <- dat[,2]

wd <- 1.4

pdf("HANSEN16-6a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f1,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",lwd=wd,ylim=c(0,.62)) 
axis(side=1,seq(-1,1,.5),lwd=wd)
dev.off()

postscript("HANSEN16-6a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f1,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",lwd=wd,ylim=c(0,.62)) 
axis(side=1,seq(-1,1,.5),lwd=wd)
dev.off()

