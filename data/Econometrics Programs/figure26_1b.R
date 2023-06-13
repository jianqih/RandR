 #############################################
### This file generates Figure 26.1b
### Binary Response Probability
#############################################
### Uses packages haven, mlogit
### Uses data file cps09mar.dta
#############################################

library(haven)
library(mlogit)

cps09mar <- read_dta("cps09mar.dta")
cps <- subset(cps09mar,(education==16)&(female==1))
cps$mstatus <- (cps$marital<=4) + 3*(cps$marital==5) + 2*(cps$marital==6)
n <- length(cps$age)

knot1 <- 40
cps$age1 <- cps$age/100
cps$age2 <- cps$age1^2 
cps$age3 <- ((cps$age1-knot1/100)^2)*(cps$age>knot1)

CPS <- dfidx(cps, shape = "wide", choice = "mstatus")

mlogit1 <- mlogit(mstatus~0|age1+age2+age3,data=CPS)
beta <- mlogit1$coef

x <- (20:80)
x1 <- x/100
x2 <- x1^2
x3 <- ((x1-knot1/100)^2)*(x>knot1)

P1 <- exp(beta[1]+x1*beta[4]+x2*beta[7]+x3*beta[10])
P2 <- exp(beta[2]+x1*beta[5]+x2*beta[8]+x3*beta[11])
P3 <- exp(beta[3]+x1*beta[6]+x2*beta[9]+x3*beta[12])
SumP <- P1 + P2 + P3 + 1
pr0 <- 1/SumP
pr1 <- P1/SumP
pr2 <- P2/SumP
pr3 <- P3/SumP

wd <- 1.4

pdf("HANSEN26-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,pr0,type="l",cex=1.1,xlim=c(22,78),ylim=c(0.035,1.015),xlab="Age",ylab="Probability",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
lines(x,pr1,lwd=wd)
lines(x,pr2,lwd=wd)
lines(x,pr3,lwd=wd)
lines(c(0,100),c(1,1),lwd=wd)
text(74,.7,"Married")
text(73,.3,"Divorced")
text(70,.12,"Never Married")
text(42,.05,"Separated")
text(76,.96,"Total")
dev.off()

postscript("HANSEN26-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,pr0,type="l",cex=1.1,xlim=c(22,78),ylim=c(0.035,1.015),xlab="Age",ylab="Probability",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
lines(x,pr1,lwd=wd)
lines(x,pr2,lwd=wd)
lines(x,pr3,lwd=wd)
lines(c(0,100),c(1,1),lwd=wd)
text(74,.7,"Married")
text(73,.3,"Divorced")
text(70,.12,"Never Married")
text(42,.05,"Separated")
text(76,.96,"Total")
dev.off()
