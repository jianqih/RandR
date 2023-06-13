#############################################
### This file generates Figure 24.6
### Quantile Regression Wage/Experience profiles
#############################################
### This file uses packages haven, quantreg
### This file uses data file cps09mar.dta
#############################################

library(haven)
library(quantreg)

cps09mar <- read_dta("cps09mar.dta")
cps <- subset(cps09mar,(education==16)&(female==1))
edu <- cps$education
wage <- log(cps$earnings/cps$hours/cps$week)
ex <- (cps$age - edu - 6)/40
ex2 <- ex^2
ex3 <- ex^3
ex4 <- ex^4
ex5 <- ex^5

q1 <- rq(wage~ex+ex2+ex3+ex4+ex5,tau=.1)
beta1 <- q1$coef
q2 <- rq(wage~ex+ex2+ex3+ex4+ex5,tau=.3)
beta2 <- q2$coef
q3 <- rq(wage~ex+ex2+ex3+ex4+ex5,tau=.5)
beta3 <- q3$coef
q4 <- rq(wage~ex+ex2+ex3+ex4+ex5,tau=.7)
beta4 <- q4$coef
q5 <- rq(wage~ex+ex2+ex3+ex4+ex5,tau=.9)
beta5 <- q5$coef

xm <- 45;
p <- 5
x <- seq(0,xm,0.5)
x1 <- x/40
x2 <- x1^2
x3 <- x1^3
x4 <- x1^4
x5 <- x1^5
f1 <- beta1[1]+x1*beta1[2]+x2*beta1[3]+x3*beta1[4]+x4*beta1[5]+x5*beta1[6]
f2 <- beta2[1]+x1*beta2[2]+x2*beta2[3]+x3*beta2[4]+x4*beta2[5]+x5*beta2[6]
f3 <- beta3[1]+x1*beta3[2]+x2*beta3[3]+x3*beta3[4]+x4*beta3[5]+x5*beta3[6]
f4 <- beta4[1]+x1*beta4[2]+x2*beta4[3]+x3*beta4[4]+x4*beta4[5]+x5*beta4[6]
f5 <- beta5[1]+x1*beta5[2]+x2*beta5[3]+x3*beta5[4]+x4*beta5[5]+x5*beta5[6]

pdf("HANSEN24-6.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f1,xlim=c(-1,xm),ylim=c(2,4),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",cex.lab=.75,cex.axis=.75,bty="n",xaxt="n")
axis(side=1,seq(-10,50,10),cex.axis=.75)
lines(x,f2)
lines(x,f3)
lines(x,f4)
lines(x,f5)
text(40,2.32,expression(q[.1]),cex=.75)
text(40,2.75,expression(q[.3]),cex=.75)
text(40,2.98,expression(q[.5]),cex=.75)
text(40,3.23,expression(q[.7]),cex=.75)
text(40,3.6,expression(q[.9]),cex=.75)
dev.off()

postscript("HANSEN24-6.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f1,xlim=c(-1,xm),ylim=c(2,4),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",cex.lab=.75,cex.axis=.75,bty="n",xaxt="n")
axis(side=1,seq(-10,50,10),cex.axis=.75)
lines(x,f2)
lines(x,f3)
lines(x,f4)
lines(x,f5)
text(40,2.32,expression(q[.1]),cex=.75)
text(40,2.75,expression(q[.3]),cex=.75)
text(40,2.98,expression(q[.5]),cex=.75)
text(40,3.23,expression(q[.7]),cex=.75)
text(40,3.6,expression(q[.9]),cex=.75)
dev.off()


