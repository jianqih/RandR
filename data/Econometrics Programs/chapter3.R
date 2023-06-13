#########################################
### This file executes the
### empirical work
### in Chapter 3
#########################################

sink("chapter3R.log")

#	Load the data and create subsamples
dat <- read.table("cps09mar.txt")
experience <- dat[,1]-dat[,4]-6
mbf <- (dat[,11]==2)&(dat[,12]<=2)&(dat[,2]==1)&(experience==12)
sam <- (dat[,11]==4)&(dat[,12]==7)&(dat[,2]==0)
dat1 <- dat[mbf,]
dat2 <- dat[sam,]

#	First regression
y <- as.matrix(log(dat1[,5]/(dat1[,6]*dat1[,7])))
x <- cbind(dat1[,4],matrix(1,nrow(dat1),1))
xx <- t(x)%*%x
xy <- t(x)%*%y
beta <- solve(xx,xy)
cat(" Regression (3.12)\n")
print(beta)

#	Second regression
y <- as.matrix(log(dat2[,5]/(dat2[,6]*dat2[,7])))
experience <- dat2[,1]-dat2[,4]-6
exp2 <- (experience^2)/100
x <- cbind(dat2[,4],experience,exp2,matrix(1,nrow(dat2),1))
xx <- t(x)%*%x
xy <- t(x)%*%y
beta <- solve(xx,xy)
cat("\n Regression (3.13)\n")
print(beta)

#	Create leverage and influence
e <- y-x%*%beta
xxi <- solve(xx)
leverage <- rowSums(x*(x%*%xxi))
r <- e/(1-leverage)
d <- leverage*e/(1-leverage)
cat("\n The influence for log wage regression (3.13)\n")
print(max(abs(d)))

# regression with the restricted sample
x_r <- x[x[,2]<45,]
y_r <- y[x[,2]<45,]
beta_r <- solve(t(x_r)%*%x_r,t(x_r)%*%y_r)
cat("\n Regression (3.44)\n")
print(beta_r)

sink()