#########################################################################
##  This file generates prep work for Figure 27.3b
##  Linear Spline of Transfers on Income
#########################################################################
##  Uses packages haven, AER, quantreg
##  Uses data file CHJ2004.dta
#########################################################################

library(haven)
library(AER)
library(quantreg)

dat <- read_dta("CHJ2004.dta")

y <- (dat$tabroad + dat$tdomestic + dat$tinkind)/1000
i <- dat$income/10000
i1 <- (i-1)*(i>1)
i2 <- (i-2)*(i>2)
i5 <- (i-5)*(i>5)
i10 <- (i-10)*(i>10)
i15 <- (i-15)*(i>15)
primary <- dat$primary
somesecondary <- dat$somesecondary
secondary <- dat$secondary
someuniversity <- dat$someuniversity
university <- dat$university
age <- dat$age
female <- dat$female
married <- dat$married
child1 <- dat$child1
child7 <- dat$child7
child15 <- dat$child15
size <- dat$size
bothwork <- dat$bothwork
notemployed <- dat$notemployed
marriedf <- dat$marriedf

knots <- c(1,2,5,10,15)
kn <- length(knots)
q <- c(0,1,2,5,10,15,20)
q1 <- (q-1)*(q>1)
q2 <- (q-2)*(q>2)
q5 <- (q-5)*(q>5)
q10 <- (q-10)*(q>10)
q15 <- (q-15)*(q>15)
qn <- length(q)

# OLS
xx <- cbind(primary,somesecondary,secondary,someuniversity,university,age,female,married,child1,child7,child15,size,bothwork,notemployed,marriedf)
ols <- lm(y~i+i1+i2+i5+i10+i15+xx)
beta <- ols$coef
f1 <- beta[1]+q*beta[2]+q1*beta[3]+q2*beta[4]+q5*beta[5]+q10*beta[6]+q15*beta[7]+mean(xx%*%beta[8:22])
f1 <- f1*1000

# Tobit
tb <- tobit(y~i+i1+i2+i5+i10+i15+xx,left=0)
beta <- tb$coef
f2 <- beta[1]+q*beta[2]+q1*beta[3]+q2*beta[4]+q5*beta[5]+q10*beta[6]+q15*beta[7]+mean(xx%*%beta[8:22])
f2 <- f2*1000

# LAD
lad <- rq(y~i+i1+i2+i5+i10+i15+xx,tau=0.5)
beta <- lad$coef
f3 <- beta[1]+q*beta[2]+q1*beta[3]+q2*beta[4]+q5*beta[5]+q10*beta[6]+q15*beta[7]+mean(xx%*%beta[8:22])
f3 <- f3*1000

# CLAD
y0 <- Curv(y,rep(0,length(y)))
clad <- crq(y0~i+i1+i2+i5+i10+i15+xx,tau=0.5,method="Pow")
beta <- clad$coef
f4 <- beta[1]+q*beta[2]+q1*beta[3]+q2*beta[4]+q5*beta[5]+q10*beta[6]+q15*beta[7]+mean(xx%*%beta[8:22])
f4 <- f4*1000
qq <- q*10000

dat = matrix(0,qn,5)
dat[,1] = qq
dat[,2] = f1
dat[,3] = f2
dat[,4] = f3
dat[,5] = f4
write.table(dat,file="figure27_3b.txt",sep="\t",row.names=FALSE,col.names=FALSE)


