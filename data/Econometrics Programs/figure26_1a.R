 #############################################
### This file generates Figure 26.1a
### Multiple Choice -- Marriage Status Example
#############################################
### Uses package haven
### Uses data file cps09mar.dta
#############################################

library(haven)

cps09mar <- read_dta("cps09mar.dta")
edu <- cps09mar$education
cps <- subset(cps09mar,(education==16)&(female==1))
age <- cps$age
marital <- cps$marital
married <- (marital<=4)
divorced <- (marital==5)
separated <- (marital==6)
single <- (marital==7)
n <- length(age)

knot1 <- 40
age1 <- age/100
age2 <- age1^2
age3 <- ((age1-knot1/100)^2)*(age>knot1)

probit0 <- glm(single~age1+age2+age3,family=binomial(link="probit"))
probit1 <- glm(married~age1+age2+age3,family=binomial(link="probit"))
probit2 <- glm(separated~age1+age2+age3,family=binomial(link="probit"))
probit3 <- glm(divorced~age1+age2+age3,family=binomial(link="probit"))
beta0 <- probit0$coef
beta1 <- probit1$coef
beta2 <- probit2$coef
beta3 <- probit3$coef

x <- (20:80)
x1 <- x/100
x2 <- x1^2
x3 <- ((x1-knot1/100)^2)*(x>knot1)

pr0 <- pnorm(beta0[1]+x1*beta0[2]+x2*beta0[3]+x3*beta0[4])
pr1 <- pnorm(beta1[1]+x1*beta1[2]+x2*beta1[3]+x3*beta1[4])
pr2 <- pnorm(beta2[1]+x1*beta2[2]+x2*beta2[3]+x3*beta2[4])
pr3 <- pnorm(beta3[1]+x1*beta3[2]+x2*beta3[3]+x3*beta3[4])
prt <- pr0+pr1+pr2+pr3

wd <- 1.4

pdf("HANSEN26-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,pr0,type="l",xlim=c(20,80),ylim=c(0.035,1.015),xlab="Age",ylab="Probability",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
lines(x,pr1,lwd=wd)
lines(x,pr2,lwd=wd)
lines(x,pr3,lwd=wd)
lines(x,prt,lwd=wd)
text(74,.72,"Married")
text(73,.32,"Divorced")
text(70,.11,"Never Married")
text(44,.05,"Separated")
text(76,.99,"Total")
dev.off()

postscript("HANSEN26-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,pr0,type="l",xlim=c(20,80),ylim=c(0.035,1.015),xlab="Age",ylab="Probability",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
lines(x,pr1,lwd=wd)
lines(x,pr2,lwd=wd)
lines(x,pr3,lwd=wd)
lines(x,prt,lwd=wd)
text(74,.72,"Married")
text(73,.32,"Divorced")
text(70,.11,"Never Married")
text(44,.05,"Separated")
text(76,.99,"Total")
dev.off()