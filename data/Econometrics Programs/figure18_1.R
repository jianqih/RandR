#####################################
### This file generates Figure 18.1
### Plots of log Liquor Sales per capita
#####################################
### Uses package haven
### Uses dataset BMN2016.dta
#########################################

library(haven)

dat <- read_dta("BMN2016.dta")
dat06 <- subset(dat,id==6)
dat19 <- subset(dat,id==19)
dat36 <- subset(dat,id==36)

n <- length(dat06$logliq)
liquor <- matrix(0,n,3)
liquor[,1] <- dat06$logliq
liquor[,2] <- dat36$logliq
liquor[,3] <- dat19$logliq
states <- ts(liquor,frequency=1,start=c(1970,1))

pdf("HANSEN18-1.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(states,plot.type="single",ylab="",xlab="",lty=1,bty="n",xaxt="n",yaxt="n")
axis(side=1,seq(1960,2010,10),cex.axis=.75)
axis(side=2,seq(-1,.6,.2),cex.axis=.75)
text(1995,-.1,"California",cex=.75)
text(1980,.1,"New York",cex=.75)
text(1975,-.3,"Iowa",cex=.75)
title(ylab="Log Per Capita Sales", cex.lab=0.75)
dev.off()

postscript("HANSEN18-1.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(states,plot.type="single",ylab="",xlab="",lty=1,bty="n",xaxt="n",yaxt="n")
axis(side=1,seq(1960,2010,10),cex.axis=.75)
axis(side=2,seq(-1,.6,.2),cex.axis=.75)
text(1995,-.1,"California",cex=.75)
text(1980,.1,"New York",cex=.75)
text(1975,-.3,"Iowa",cex=.75)
title(ylab="Log Per Capita Sales", cex.lab=0.75)
dev.off()
