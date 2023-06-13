#########################################
### This file calculates the leverage numbers 
### discussed in the Section on leverage
### 
### Uses data file cps90mar.txt
#########################################

dat <- read.table("../../data/cps09mar/cps09mar.txt")
sam <- (dat[,11]==4)&(dat[,12]==7)&(dat[,2]==0)
dat1 <- dat[sam,]
n <- nrow(dat1)

education <- dat1[,4]
experience <- dat1[,1]-education-6
exp2 <- (experience^2)/100
exp3 <- (experience/50)^3
exp4 <- (experience/50)^4
exp5 <- (experience/50)^5
i1 <- matrix(1,n,1)

x1 <- cbind(i1,education,experience)
x2 <- cbind(i1,education,experience,exp2)
x3 <- cbind(i1,education,experience,exp2,exp3)
x4 <- cbind(i1,education,experience,exp2,exp3,exp4)
x5 <- cbind(i1,education,experience,exp2,exp3,exp4,exp5)

leverage <- matrix(0,1,5);
leverage[1,1] <- max(rowSums(x1*(x1%*%solve(t(x1)%*%x1))))
leverage[1,2] <- max(rowSums(x2*(x2%*%solve(t(x2)%*%x2))))
leverage[1,3] <- max(rowSums(x3*(x3%*%solve(t(x3)%*%x3))))
leverage[1,4] <- max(rowSums(x4*(x4%*%solve(t(x4)%*%x4))))
leverage[1,5] <- max(rowSums(x5*(x5%*%solve(t(x5)%*%x5))))

print(leverage)
