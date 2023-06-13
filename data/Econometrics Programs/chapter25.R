#########################################################################
##  This file executes the empirical work reported in Chapter 25
#########################################################################
### Uses package haven
### Uses data file cps09mar.dta
#########################################


library(haven)

cps09mar <- read_dta("cps09mar.dta")
edu <- cps09mar$education
cps <- subset(cps09mar,female==1)
age <- cps$age
edu <- cps$education
black <- (cps$race == 2)
hispanic <- cps$hisp
marital <- cps$marital
married <- (marital<=3)
union <- cps$union
n <- length(age)
x <- (19:80)

knot1 <- .35
knot2 <- .4
X1 <- age/100
X2 <- X1^2 
X3 <- ((X1-knot1)^2)*(X1>knot1)
X4 <- ((X1-knot2)^2)*(X1>knot2)

probit4 <- glm(married~X1+X2+X3+X4+edu+black+hispanic,family=binomial(link="probit"))
beta <- probit4$coef

x1 <- x/100
x2 <- x1^2
x3 <- ((x1-knot1)^2)*(x1>knot1)
x4 <- ((x1-knot2)^2)*(x1>knot2)
pr <- pnorm(beta[1]+x1*beta[2]+x2*beta[3]+x3*beta[4]+x4*beta[5])

xn <- length(x)
np <- matrix(NA,xn,1)
for (i in 1:xn) {
  a <- (age==(i+18))
  if (sum(a)>0)  np[i] <- mean(married[a])
}

probit1 <- glm(union~age+edu+black+hispanic,family=binomial(link="probit"))
