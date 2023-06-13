#########################################################################
##  This file generates Figure 16.3ab
##  Dickey-Fuller Distributions
#########################################################################
### Uses files figure16_3a.txt, figure16_3b.txt
#########################################

dat <- read.table("figure16_3a.txt")
x <- dat[,1]
f1 <- dat[,2]
f2 <- dat[,3]
f3 <- dat[,4]

wd <- 1.4

pdf("HANSEN16-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f1,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",ylim=c(0,.25),xlim=c(-30,4),lwd=wd) 
axis(side=1,seq(-30,5,5),lwd=wd)
lines(x,f2,lwd=wd)
lines(x,f3,lwd=wd)
text(-6,.2,"No Intercept")
text(-9,.1,"Intercept")
text(-22,.025,"Trend")
dev.off()

postscript("HANSEN16-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f1,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",ylim=c(0,.25),xlim=c(-30,4),lwd=wd) 
axis(side=1,seq(-30,5,5),lwd=wd)
lines(x,f2,lwd=wd)
lines(x,f3,lwd=wd)
text(-6,.2,"No Intercept")
text(-9,.1,"Intercept")
text(-22,.025,"Trend")
dev.off()

# T Densities

dat <- read.table("figure16_3b.txt")
x <- dat[,1]
g1 <- dat[,2]
g2 <- dat[,3]
g3 <- dat[,4]

pdf("HANSEN16-3b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,g1,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(-5,3),ylim=c(0,.6),lwd=wd) 
axis(side=1,seq(-5,3,1),lwd=wd)
lines(x,g2,lwd=wd)
lines(x,g3,lwd=wd)
text(1.5,.3,"No Intercept")
text(-.48,.48,"Intercept")
text(-1.5,.57,"Trend")
dev.off()

postscript("HANSEN16-3b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,g1,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(-5,3),ylim=c(0,.6),lwd=wd) 
axis(side=1,seq(-5,3,1),lwd=wd)
lines(x,g2,lwd=wd)
lines(x,g3,lwd=wd)
text(1.5,.3,"No Intercept")
text(-.48,.48,"Intercept")
text(-1.5,.57,"Trend")
dev.off()


