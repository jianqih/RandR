#####################################
### This file generates Figure 16.7
### Interest Rates
#####################################
### Uses package haven
### Uses dataset FRED-QD.dta 
#########################################

library(haven)
setwd("~/project_set/rrp/data/Econometrics Programs")

data <- read_dta("FRED-QD.dta")
x <- as.matrix(data$gs10)
y <- as.matrix(data$tb3ms)
spread <- x - y
n <- length(y)
tr <- seq(1959.25,2018,.25)
z <- matrix(mean(spread),n,1)

wd <- 1.4

pdf("HANSEN16-7a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(tr,x,type="l",xaxs="i",yaxs="i",ylab="Percentage",xlab="",bty="n",yaxt="n",xaxt="n",ylim=c(0,16),lwd=wd) 
lines(tr,y,lty=5,lwd=wd)
axis(side=1,seq(1950,2030,10),lwd=wd)
axis(side=2,seq(0,18,5),lwd=wd)
legend("topright",c("10-Year Rate","3-Month Rate"), lty=c(1,5),lwd=wd,bty="n")
dev.off()

postscript("HANSEN16-7a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(tr,x,type="l",xaxs="i",yaxs="i",ylab="Percentage",xlab="",bty="n",yaxt="n",xaxt="n",ylim=c(0,16),lwd=wd) 
lines(tr,y,lty=5,lwd=wd)
axis(side=1,seq(1950,2030,10),lwd=wd)
axis(side=2,seq(0,18,2),lwd=wd)
legend("topright",c("10-Year Rate","3-Month Rate"), lty=c(1,5),lwd=wd,bty="n")
dev.off()

pdf("HANSEN16-7b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(tr,spread,type="l",xlab="",ylab="Percentage",bty="n",yaxt="n",xaxt="n",xaxs="i",yaxs="i",ylim=c(-2,4),lwd=wd)
lines(tr,z,lty=2,lwd=wd)
axis(side=1,seq(1950,2030,10),lwd=wd)
axis(side=2,seq(-2,4,1),lwd=wd)
dev.off()

postscript("HANSEN16-7b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(tr,spread,type="l",xlab="",ylab="Percentage",bty="n",yaxt="n",xaxt="n",xaxs="i",yaxs="i",ylim=c(-2,4),lwd=wd)
lines(tr,z,lty=2,lwd=wd)
axis(side=1,seq(1950,2030,10),lwd=wd)
axis(side=2,seq(-2,4,1),lwd=wd)
dev.off()

