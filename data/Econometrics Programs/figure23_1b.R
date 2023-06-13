#####################################
### This file generates
### Figure 23.1b Regression Kink Regression Function
### Regression Kink estimation
#####################################
### This file uses the package MASS
### This file uses the data file RR2010.txt
#####################################

library(MASS)

growth <- read.table("RR2010.txt",header=FALSE)
n <- nrow(growth)
year <- growth[2:n,1]
gdp  <- growth[2:n,3]
gdp1 <- growth[1:(n-1),3]
debt1 <-  growth[1:(n-1),2]

# Define variables
y <- gdp
n <- length(y)
x <- debt1
z <- cbind(matrix(1,n,1),gdp1) 

B <- 10000		# Number of bootstrap replications
conf <- 0.9		# Confidence level for regression confidence sets
interval <- c(10,70)	# Interval for threshold estimation

# Some useful functions
reg <- function(y,X) {
  xx <- t(X)%*%X
  xy <- t(X)%*%y
  c <- rcond(xx)
  if (c < 1e-15) b <- ginv(xx)%*%xy
  else b <- solve(xx,xy)
  return(b)
}
pos.part <- function(x) x*(x>0)
neg.part <- function(x) x*(x<0)
Xmat <- function(X,Z,g) XX <- cbind(Z,neg.part(X - g),pos.part(X - g))
treg_g <- function(y,X,Z,g) {
  XX <- Xmat(X,Z,g)
  b <- reg(y,XX)
  e <- y - XX%*%b
  s <- mean(e^2)
  return(list("beta"=b,"residual"=e,"sigma2"=s))
}
treg_opt <- function(y,X,Z,g) {
  R <- treg_g(y,X,Z,g)
  return(R$sigma2)
}
treg <- function(y,X,Z,C) {
  R1 <- optimize(treg_opt,interval=C,y=y,X=X,Z=Z)
  s <- R1$objective
  g <- R1$min
  R2 <- treg_g(y,X,Z,g)
  b <- R2$beta
  e <- R2$residual
  return(list("beta"=b,"residual"=e,"sigma2"=s,"threshold"=g))
}


R <- treg(y,x,z,interval)
sig2 <- R$sigma2
beta <- R$beta
e <- R$residual
g <- R$threshold
betag = rbind(beta,g)

k <- length(beta)
h1 <- - (x < g)*beta[k-1] - (x > g)*beta[k]
XX <- cbind(Xmat(x,z,g),h1)
Q <- solve(crossprod(XX))
S <- crossprod(XX*matrix(e,n,ncol(XX)))
V <- Q%*%S%*%Q
se <- as.matrix(sqrt(diag(V)))


cat("Estimation Interval")
print(interval)
cat("Bootstrap iterations, Confidence Level")
print(cbind(B,conf))
cat("Coefficients and se's","\n")
print(cbind(betag,se),digits=2)

# Linear Null Model
  X0 <- cbind(z,x)
  sig0 <- mean((y - X0%*%reg(y,X0))^2)
  F <- n*(sig0-sig2)/sig2

# Multiplier Bootstrap 
B <- 1000
Fstore <-matrix(0,B,1)
for (i in 1:B) {
  yi <- e*((runif(n)>0.5)*2-1)
  sig0i <- mean((yi - X0%*%reg(yi,X0))^2)
  Ri <- treg(yi,x,z,interval)
  sig2i <- Ri$sigma2
  Fstore[i] <- n*(sig0i-sig2i)/sig2i
}
pvalue <- mean(Fstore > F)

cat("F test for Significance: F, p-value \n")
print(cbind(F,pvalue))


# Regression line and confidence intervals

dx <- seq(0,121,by=1)
nx = length(dx)
mz <- mean(gdp1)
G <- cbind(matrix(1,nx,1),matrix(mz,nx,1),neg.part(dx-g),pos.part(dx-g))
mf <- G%*%beta

conf <- .9
mu1 <- y - e
rstore <- matrix(0,nx,B)
hg <- -(beta[k-1]*(dx<g) + beta[k]*(dx>g))
for (i in 1:B) {
  ys <- mu1 + e*((runif(n)>0.5)*2-1)
  Rs <- treg(ys,x,z,interval)
  bs <- Rs$beta
  gs <- Rs$threshold
  rstore[,i] <- G%*%(bs-beta) + hg*(gs-g)
}
astore <- abs(rstore)
q <- apply(astore,1,quantile,conf)
mf1 <- mf-q
mf2 <- mf+q

wd <- 1.4

pdf("HANSEN23-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(dx,mf,type="l",ylab="GDP Growth Rate",xlab="Debt/GDP",xaxs="i",yaxs="i",ylim=c(-6,7),yaxt="n",lwd=wd,bty="n")
polygon(c(dx,rev(dx)),c(mf1,rev(mf2)),border=NA,col=gray(.8))
lines(dx,mf,lwd=wd)
axis(side=1,lwd=wd)
axis(side=2,seq(-8,8,2),lwd=wd)
yk <- beta[1]+beta[2]*mz
points(g,yk,pch=22,cex=1.3)
dev.off()

postscript("HANSEN23-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(dx,mf,type="l",ylab="GDP Growth Rate",xlab="Debt/GDP",xaxs="i",yaxs="i",ylim=c(-6,7),yaxt="n",lwd=wd,bty="n")
polygon(c(dx,rev(dx)),c(mf1,rev(mf2)),border=NA,col=gray(.8))
lines(dx,mf,lwd=wd)
axis(side=1,lwd=wd)
axis(side=2,seq(-8,8,2),lwd=wd)
yk <- beta[1]+beta[2]*mz
points(g,yk,pch=22,cex=1.3)
dev.off()


