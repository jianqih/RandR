#####################################
### This file generates Figure 29.2
### Ridge Regression Application
#####################################
### Uses package haven
### Uses data file cps09mar.dta
#####################################

library(haven)

cps <- read_dta("cps09mar.dta")
cps <- cps[cps$race==4,]
cps <- cps[cps$female==0,]
cps <- cps[cps$education>=16,]

attach(cps)
 y  <- matrix(log(earnings/hours/week))
 ex <- age - education - 6
detach(cps)
n <- nrow(y)

ex0 <- ex - mean(ex)
x1 <- ex0/sd(ex0)
x2 <- (ex0^2 - mean(ex0^2))/sd(ex0^2)
x3 <- (ex0^3 - mean(ex0^3))/sd(ex0^3)
x4 <- (ex0^4 - mean(ex0^4))/sd(ex0^4)
x5 <- (ex0^5 - mean(ex0^5))/sd(ex0^5)
X <- cbind(matrix(1,n,1),x1,x2,x3,x4,x5)
k <- ncol(X)

z <- (0:40)
lz <- length(z)
z0 <- z - mean(ex)
z1 <- z0/sd(ex0)
z2 <- (z0^2 - mean(ex0^2))/sd(ex0^2)
z3 <- (z0^3 - mean(ex0^3))/sd(ex0^3)
z4 <- (z0^4 - mean(ex0^4))/sd(ex0^4)
z5 <- (z0^5 - mean(ex0^5))/sd(ex0^5)
Z <- cbind(matrix(1,lz,1),z1,z2,z3,z4,z5)

xx <- crossprod(X)
xxols <- solve(xx)
xy <- crossprod(X,y)
betaols <- xxols%*%xy
eols <- y - X%*%betaols
rols <- eols/(1-rowSums(X*(X%*%xxols)))
omegaols <- crossprod(X*(rols%*%matrix(1,1,k)))
vols <- xxols%*%omegaols%*%xxols

fols <- Z%*%betaols
sols <- sqrt(rowSums(Z*(Z%*%vols)))
Uols <- fols + sols*1.96
Lols <- fols - sols*1.96


G <- 1000
cv <- matrix(0,G,1)
lambda <- (seq(1:G)-1)*60/G
K <- diag(k)
K[1,1] <- 0

for (i in 1:G) {
  xxi <- solve(xx + K*lambda[i])
  ei <- y - X%*%xxi%*%xy
  r <- ei/(1-rowSums(X*(X%*%xxi)))
  cv[i] <- sum(r^2)
}

i <- which.min(cv)
lambdamin <- lambda[i]
cvmin <- cv[i]
xxridge <- solve(xx + K*lambdamin)
betaridge <- xxridge%*%xy
eridge <- y - X%*%betaridge
rridge <- eridge/(1-rowSums(X*(X%*%xxridge)))
omegaridge <- crossprod(X*(rridge%*%matrix(1,1,k)))
vridge <- xxridge%*%omegaridge%*%xxridge

fridge <- Z%*%betaridge
sridge <- sqrt(rowSums(Z*(Z%*%vridge)))
Uridge <- fridge + sridge*1.96
Lridge <- fridge - sridge*1.96

wd <- 1.4

pdf("HANSEN29-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(lambda,cv,type="l",lty=1,xaxs="i",xlim=c(0,60),ylim=c(442.67,444.5),yaxs="i",ylab="Cross-Validation Criterion",xlab=expression(lambda),bty="n",yaxt="n",lwd=wd)
points(lambdamin,cvmin,pch=19,col="black",cex=.7)
axis(side=2,seq(442.5,444.5,.5),lwd=wd)
axis(side=1,lwd=wd)
text(lambdamin,442.85,expression(hat(lambda)))
dev.off()

postscript("HANSEN29-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(lambda,cv,type="l",lty=1,xaxs="i",xlim=c(0,60),ylim=c(442.67,444.5),yaxs="i",ylab="Cross-Validation Criterion",xlab=expression(lambda),bty="n",yaxt="n",lwd=wd)
points(lambdamin,cvmin,pch=19,col="black",cex=.7)
axis(side=2,seq(442.5,444.5,.5),lwd=wd)
axis(side=1,lwd=wd)
text(lambdamin,442.85,expression(hat(lambda)))
dev.off()


pdf("HANSEN29-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(z,fols,type="l",lty=5,xaxs="i",yaxs="i",ylab="Log Wage",ylim=c(2.8,3.6),xlim=c(0,40),xlab="Experience (Years)",bty="n",lwd=wd)
polygon(c(z,rev(z)),c(Uridge,rev(Lridge)),col=gray(.8),border=NA,lwd=wd)
lines(z,fols,lty=5,lwd=wd)
lines(z,fridge,lty=1,lwd=wd)
legend("bottomright",legend=c("Least Squares","Ridge Regression"),lty=c(5,1),lwd=wd,bty="n")
axis(side=1,lwd=wd)
axis(side=2,lwd=wd)
dev.off()

postscript("HANSEN29-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(z,fols,type="l",lty=5,xaxs="i",yaxs="i",ylab="Log Wage",ylim=c(2.8,3.6),xlim=c(0,40),xlab="Experience (Years)",bty="n",lwd=wd)
polygon(c(z,rev(z)),c(Uridge,rev(Lridge)),col=gray(.8),border=NA,lwd=wd)
lines(z,fols,lty=5,lwd=wd)
lines(z,fridge,lty=1,lwd=wd)
legend("bottomright",legend=c("Least Squares","Ridge Regression"),lty=c(5,1),lwd=wd,bty="n")
axis(side=1,lwd=wd)
axis(side=2,lwd=wd)
dev.off()

