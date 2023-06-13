#########################################################################
##  This file generates Figure 22.2b
##  Density Discontinuity Plot
#########################################################################
##  This file uses the package haven
##  This file uses the data file LM2007.dta
#########################################################################

library(haven)

dat <- read_dta("LM2007.dta")
x <- dat$povrate60

c <- 59.1984
b <- seq(15,85,2) + 0.1984

wd <- 1.4

pdf("HANSEN21-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
hist(x,breaks=b,ylab="Histogram",xlab="Poverty Rate",main="",freq=TRUE,xlim=c(15,85),xaxt="n",yaxt="n",lwd=wd,bty="n")
abline(v=c,lwd=2*wd)
axis(side=1,seq(10,100,10),lwd=wd)
axis(side=2,seq(0,200,40),lwd=wd)
text(67,160,"Cut-Off")
dev.off()

postscript("HANSEN21-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
hist(x,breaks=b,ylab="Histogram",xlab="Poverty Rate",main="",freq=TRUE,xlim=c(15,85),xaxt="n",yaxt="n",lwd=wd,bty="n")
abline(v=c,lwd=2*wd)
axis(side=1,seq(10,100,10),lwd=wd)
axis(side=2,seq(0,200,40),lwd=wd)
text(67,160,"Cut-Off")
dev.off()
