#############################################
### This file generates Figure 25.1
### Binary Response Probability
#############################################
### Uses package haven
### Uses data file cps09mar.dta
#############################################

library(haven)

cps09mar <- read_dta("cps09mar.dta")
edu <- cps09mar$education
cps <- subset(cps09mar,(education==16)&(female==0))
age <- cps$age
marital <- cps$marital
married <- (marital<=4)
n <- length(age)

knot1 <- 40
knot2 <- 60
age1 <- age/100
age2 <- age1^2
age3 <- ((age1-knot1/100)^2)*(age>knot1)
age4 <- ((age1-knot2/100)^2)*(age>knot2)

probit4 <- glm(married~age1+age2+age3+age4,family=binomial(link="probit"))
beta <- probit4$coef

x <- (19:80)
x1 <- x/100
x2 <- x1^2
x3 <- ((x1-knot1/100)^2)*(x>knot1)
x4 <- ((x1-knot2/100)^2)*(x>knot2)
pr <- pnorm(beta[1]+x1*beta[2]+x2*beta[3]+x3*beta[4]+x4*beta[5])

xn <- length(x)
np <- matrix(NA,xn,1)
for (i in 1:xn) {
  a <- (age==(i+18))
  if (sum(a)>0)  np[i] <- mean(married[a])
}

wd <- 1.4

pdf("HANSEN25-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,np,type="p",cex=.6,pch=19,xlim=c(15,80),ylim=c(-.1,1.1),xlab="Age",ylab="Probability",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
lines(x,pr,lwd=wd)
abline(h=0)
abline(h=1)
dev.off()

postscript("HANSEN25-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,np,type="p",cex=.6,pch=19,xlim=c(15,80),ylim=c(-.1,1.1),xlab="Age",ylab="Probability",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
lines(x,pr,lwd=wd)
abline(h=0)
abline(h=1)
dev.off()



ols1 <- lm(married~age)
betaols1 <- ols1$coef
pols1 <- betaols1[1] + x*betaols1[2]

ols2 <- lm(married~age1+age2+age3+age4)
betaols2 <- ols2$coef
pols2 <- betaols2[1] + x1*betaols2[2] + x2*betaols2[3] + x3*betaols2[4] + x4*betaols2[5]

probit1 <- glm(married~age,family=binomial(link="probit"))
betap <- probit1$coef
pr1 <- pnorm(betap[1]+x*betap[2])

pdf("HANSEN25-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,np,type="p",cex=0.4,pch=19,xlim=c(15,80),ylim=c(-.1,1.1),xlab="Age",ylab="Probability",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
lines(x,pols1,lty=6,lwd=wd)
lines(x,pols2,lty=5,lwd=wd)
lines(x,pr1,lty=2,lwd=wd)
lines(x,pr,lty=1,lwd=wd)
abline(h=0)
abline(h=1)
legend("right",legend=c("Linear","Probit","Linear Series","Probit Series"),lty=c(6,2,5,1),lwd=wd,bty="n")
dev.off()

postscript("HANSEN25-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,np,type="p",cex=0.4,pch=19,xlim=c(15,80),ylim=c(-.1,1.1),xlab="Age",ylab="Probability",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
lines(x,pols1,lty=6,lwd=wd)
lines(x,pols2,lty=5,lwd=wd)
lines(x,pr1,lty=2,lwd=wd)
lines(x,pr,lty=1,lwd=wd)
abline(h=0)
abline(h=1)
legend("right",legend=c("Linear","Probit","Linear Series","Probit Series"),lty=c(6,2,5,1),lwd=wd,bty="n")
dev.off()
