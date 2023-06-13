#####################################
### This file generates Figures 14.1 - 14.2
### Plots of GDP, exchange rate, Treasury Bill, oil price, unemployment rate, consumption growth, inflation, stock return
#####################################
### Uses package haven
### Uses datasets FRED-QD.dta and FRED-MD-dta
#########################################

library(haven)

data <- read_dta("FRED-QD.dta")
dat <- cbind(data$gdpc1,data$pcndx)
datt <- ts(dat,frequency=4,start=c(1959,1))
gdp <- datt[,1]/1000
c <- datt[,2]
c <- (((c/lag(c,-1)))-1)*100

wd <- 1.4

pdf("HANSEN14-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(gdp,xlab="",ylab="Trillions of Chained 2012 Dollars",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(2,20,2),lwd=wd)
dev.off()

postscript("HANSEN14-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(gdp,xlab="",ylab="Trillions of Chained 2012 Dollars",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(2,20,2),lwd=wd)
dev.off()

pdf("HANSEN14-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(c,xlab="",ylab="Percentage",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(-3,5,1),lwd=wd)
dev.off()

postscript("HANSEN14-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(c,xlab="",ylab="Percentage",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(-3,5,1),lwd=wd)
dev.off()

data <- read_dta("FRED-MD.dta")
dat <- cbind(data$excausx,data$gs10,data$oilpricex,data$unrate,data$cpiaucsl,data$sp500)
datt <- ts(dat,frequency=12,start=c(1959,1))
ex <- datt[,1]
gs10 <- datt[,2]
oil <- datt[,3]
ur <- datt[,4]
inf <- (((datt[,5]/lag(datt[,5],-1)))-1)*100
r <- (((datt[,6]/lag(datt[,6],-1)))-1)*100

pdf("HANSEN14-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(ex,xlab="",ylab="Canadian Dollar/U.S. Dollar",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(0,1.6,.2),lwd=wd)
dev.off()

postscript("HANSEN14-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(ex,xlab="",ylab="Canadian Dollar/U.S. Dollar",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(0,1.6,.2),lwd=wd)
dev.off()

pdf("HANSEN14-1c.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(gs10,xlab="",ylab="Percentage",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(0,16,2),lwd=wd)
dev.off()

postscript("HANSEN14-1c.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(gs10,xlab="",ylab="Percentage",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(0,16,2),lwd=wd)
dev.off()

pdf("HANSEN14-1d.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(oil,xlab="",ylab="2012 Dollars per Barrel",yaxt="n",xaxt="n",bty="n",ylim=c(4,120),lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(0,120,20),lwd=wd)
dev.off()

postscript("HANSEN14-1d.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(oil,xlab="",ylab="2012 Dollars per Barrel",yaxt="n",xaxt="n",bty="n",ylim=c(4,120),lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(0,120,20),lwd=wd)
dev.off()

pdf("HANSEN14-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(ur,xlab="",ylab="Percentage",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(3,15,1),lwd=wd)
dev.off()

postscript("HANSEN14-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(ur,xlab="",ylab="Percentage",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(3,15,1),lwd=wd)
dev.off()

pdf("HANSEN14-2c.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(inf,xlab="",ylab="Percentage",xaxt="n",yaxt="n",bty="n",lwd=wd,ylim=c(-0.8,1.7))
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(-2,2,.5),lwd=wd)
dev.off()

postscript("HANSEN14-2c.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(inf,xlab="",ylab="Percentage",xaxt="n",yaxt="n",bty="n",lwd=wd,ylim=c(-0.8,1.7))
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(-2,2,.5),lwd=wd)
dev.off()

pdf("HANSEN14-2d.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(r,xlab="",ylab="Percentage",xaxt="n",yaxt="n",bty="n",lwd=wd,ylim=c(-18,11))
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(-25,20,5),lwd=wd)
dev.off()

postscript("HANSEN14-2d.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(r,xlab="",ylab="Percentage",xaxt="n",yaxt="n",bty="n",lwd=wd,ylim=c(-18,11))
axis(side=1,seq(1950,2020,10),lwd=wd)
axis(side=2,seq(-25,20,5),lwd=wd)
dev.off()
