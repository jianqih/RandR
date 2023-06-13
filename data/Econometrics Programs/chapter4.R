#########################################
### This file executes the
### empirical work
### in Chapter 4
#########################################
### Uses package haven
### Uses data files cps09mar.txt, DDK2011.dta
#########################################

library(haven)

sink("chatper4R.log")

#	Load the data and create subsamples
dat <- read.table("cps09mar.txt")
experience <- dat[,1]-dat[,4]-6
mbf <- (dat[,11]==2)&(dat[,12]<=2)&(dat[,2]==1)&(experience==12)
sam <- (dat[,11]==4)&(dat[,12]==7)&(dat[,2]==0)
dat1 <- dat[mbf,]
dat2 <- dat[sam,]

# Table 4.1
y <- as.matrix(log(dat1[,5]/(dat1[,6]*dat1[,7])))
x <- cbind(dat1[,4],matrix(1,nrow(dat1),1))
beta <- solve(t(x)%*%x,t(x)%*%y)
cat(" Table 4.1 (Regression (3.12)) \n")
cat(" Coefficient estimates \n")
print(t(beta))

e <- y-x%*%beta
leverage <- rowSums(x*(x%*%solve(t(x)%*%x)))

n <- nrow(y)
k <- ncol(x)
a <- n/(n-k)
sig2 <- c(t(e) %*% e)/(n-k)
u1 <- x*(e%*%matrix(1,1,k))
u2 <- x*((e/sqrt(1-leverage))%*%matrix(1,1,k))
u3 <- x*((e/(1-leverage))%*%matrix(1,1,k))
xx <- solve(t(x)%*%x)
v0 <- xx*sig2
v1 <- xx %*% (t(u1)%*%u1) %*% xx
v1a <- a * xx %*% (t(u1)%*%u1) %*% xx
v2 <- xx %*% (t(u2)%*%u2) %*% xx
v3 <- xx %*% (t(u3)%*%u3) %*% xx
s0 <- sqrt(diag(v0)) # Homoskedastic formula
s1 <- sqrt(diag(v1)) # White formula
s1a <- sqrt(diag(v1a)) # HC1 formula
s2 <- sqrt(diag(v2)) # HC2 formula
s3 <- sqrt(diag(v3)) # HC3 formula
cat(" Homoskedastic ste \n")
print(s0)
cat(" White ste \n")
print(s1)
cat(" Scaled White ste \n")
print(s1a)
cat(" Andrews ste \n")
print(s2)
cat(" HHD ste \n")
print(s3)

# Equation 3.13
y <- as.matrix(log(dat2[,5]/(dat2[,6]*dat2[,7])))
experience <- dat2[,1]-dat2[,4]-6
exp2 <- (experience^2)/100
x <- cbind(dat2[,4],experience,exp2,matrix(1,nrow(dat2),1))
beta <- solve(t(x)%*%x,t(x)%*%y)
cat("\n Regression (3.13) \n")
cat(" Coefficient estimates \n")
print(t(beta))

e <- y-x%*%beta
leverage <- rowSums(x*(x%*%solve(t(x)%*%x)))

n <- nrow(y)
k <- ncol(x)
a <- n/(n-k)
sig2 <- c(t(e) %*% e)/(n-k)
u1 <- x*(e%*%matrix(1,1,k))
u2 <- x*((e/sqrt(1-leverage))%*%matrix(1,1,k))
u3 <- x*((e/(1-leverage))%*%matrix(1,1,k))
xx <- solve(t(x)%*%x)
v0 <- xx*sig2
v1 <- xx %*% (t(u1)%*%u1) %*% xx
v1a <- a * xx %*% (t(u1)%*%u1) %*% xx
v2 <- xx %*% (t(u2)%*%u2) %*% xx
v3 <- xx %*% (t(u3)%*%u3) %*% xx
s0 <- sqrt(diag(v0)) # Homoskedastic formula
s1 <- sqrt(diag(v1)) # White formula
s1a <- sqrt(diag(v1a)) # HC1 formula
s2 <- sqrt(diag(v2)) # HC2 formula
s3 <- sqrt(diag(v3)) # HC3 formula
cat(" Homoskedastic ste \n")
print(s0)
cat(" White se \n")
print(s1)
cat(" HC1 se \n")
print(s1a)
cat(" HC2 se \n")
print(s2)
cat(" HC3 se \n")
print(s3)


# Table 4.2.
edu12 <- (dat[,4]>11)
dat3 <- dat[edu12,]

marriedF <- (dat3[,12]<=3)&(dat3[,2]==1)
marriedM <- (dat3[,12]<=3)&(dat3[,2]==0)
unionF <- (dat3[,8]==1)&(dat3[,2]==1)
unionM <- (dat3[,8]==1)&(dat3[,2]==0)
fmarriedF <- (dat3[,12]<=6)&(dat3[,12]>3)&(dat3[,2]==1)
fmarriedM <- (dat3[,12]<=6)&(dat3[,12]>3)&(dat3[,2]==0)
black <- (dat3[,11]==2)
american_indian <- (dat3[,11]==3)
asian <- (dat3[,11]==4)
mixed <- (dat3[,11]>=6)

y <- as.matrix(log(dat3[,5]/(dat3[,6]*dat3[,7])))
experience <- dat3[,1]-dat3[,4]-6
exp2 <- (experience^2)/100
x <- cbind(dat3[,4],experience,exp2,dat3[,2],
           unionF,unionM,marriedF,marriedM,fmarriedF,fmarriedM,
           dat3[,3],black,american_indian,asian,mixed,matrix(1,nrow(dat3),1))
beta <- solve(t(x)%*%x,t(x)%*%y)

k <- ncol(x)
e <- y-x%*%beta
leverage <- rowSums(x*(x%*%solve(t(x)%*%x)))
u2 <- x*((e/sqrt(1-leverage))%*%matrix(1,1,k))
xx <- solve(t(x)%*%x)
v2 <- xx %*% (t(u2)%*%u2) %*% xx
s2 <- sqrt(diag(v2)) # HC2
cat("\n Table 4.2: Log Wage Regression with HC2 se\n")
outcome_mat = cbind(beta,matrix(s2))
colnames(outcome_mat) <- c("Coef.","HC2 se")
print(outcome_mat)

# DDK (2011)

# Load the data DDK2011 and create variables
data <- read_dta("DDK2011.dta")
y <- scale(as.matrix(data$totalscore))
n <- nrow(y)
x <- cbind(as.matrix(data$tracking),matrix(1,n,1))
schoolid <- as.matrix(data$schoolid)
k <- ncol(x)
xx <- t(x)%*%x
invx <- solve(xx)
beta <- solve(xx,t(x)%*%y)
xe <- x*rep(y-x%*%beta,times=k)
# Clustered robust standard error
xe_sum <- rowsum(xe,schoolid)
G <- nrow(xe_sum)
omega <- t(xe_sum)%*%xe_sum
scale <- G/(G-1)*(n-1)/(n-k)
V_clustered <- scale*invx%*%omega%*%invx
se_clustered <- sqrt(diag(V_clustered))

cat("\n Regression (4.52) of test scores on the tracking dummy \n")
cat(" Coefficient estimates \n")
print(t(beta))
cat(" Clustered ste \n")
print(se_clustered)

sink()
