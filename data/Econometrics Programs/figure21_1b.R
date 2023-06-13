#########################################################################
##  This file generates Figure 21.1b, Table 21.1 Baseline, and equation (21.5)
##  Head Start Regression Discontinuity
#########################################################################
##  The package haven is used
##  The data file LM2007.dta is used
#########################################################################

library(haven)

dat <- read_dta("LM2007.dta")
y <- dat$mort_age59_related_postHS
x <- dat$povrate60

h = 8
c <- 59.1984
T <- as.numeric(x >= c)
y1 <- y[T==0]
y2 <- y[T==1]
x1 <- x[T==0]
x2 <- x[T==1]
n1 <- length(y1)
n2 <- length(y2)
n <- n1+n2

TriKernel <- function(x) {
  s6 <- sqrt(6)
  ax <- abs(x)
  f <- (1-ax/s6)*(ax < s6)/s6
  return(f)
}

LL_EST <- function(y,x,g,h) {
  G <- length(g)
  m <- matrix(0,G,1)
  z <- matrix(1,length(y),1)
  for (j in 1:G){
    xj <- x-g[j]
    K <- TriKernel(xj/h)
    zj <- cbind(z,xj)
    zz <- t(cbind(K,xj*K))
    beta <- solve(zz%*%zj,zz%*%y)
    m[j] <- beta[1]
  }
  return(m)
}

LL_Residual <- function(y,x,h) {
  n <- length(y)
  e <- matrix(0,n,1)
  z <- matrix(1,n,1)
  for (j in 1:n){
    xj <- x-x[j]
    K <- TriKernel(xj/h)
    K[j] <- 0
    zj <- cbind(z,xj)
    zz <- t(cbind(K,xj*K))
    beta <- solve(zz%*%zj,zz%*%y)
    e[j] <- y[j] - beta[1]
  }
  return(e)
}

LL_SE <- function(y,x,g,h) {
  G <- length(g)
  s <- matrix(0,G,1)
  z <- matrix(1,length(y),1)
  e <- LL_Residual(y,x,h)
  for (j in 1:G){
    xj <- x-g[j]
    K <- TriKernel(xj/h)
    zj <- cbind(z,xj)
    zz <- cbind(K,xj*K)
    ZKZ <- solve(t(zz)%*%zj)
    Ke <- K*e
    ze <- cbind(Ke,xj*Ke)
    V <- ZKZ%*%crossprod(ze)%*%ZKZ
    s[j] = sqrt(V[1,1])
  }
  return(s)
}

# RDD Estimation
g1 <- seq(15,59.2,.2)
g2 <- seq(59.2,82,.2)
G1 <- length(g1)
G2 <- length(g2)

m1 <- LL_EST(y1,x1,g1,h)
m2 <- LL_EST(y2,x2,g2,h)
s1 <- LL_SE(y1,x1,g1,h)
s2 <- LL_SE(y2,x2,g2,h)
L1 <- m1-1.96*s1
U1 <- m1+1.96*s1
L2 <- m2-1.96*s2
U2 <- m2+1.96*s2
theta <- m2[1]-m1[G1]
setheta <- sqrt(s1[G1]^2 + s2[1]^2)
tstat <- theta/setheta
pvalue <- 2*(1-pnorm(abs(tstat)))

h0 <- h*sqrt(3)
w <- abs(x-c)<=h
y0 <- y[w]
x0 <- x[w]
T0 <- T[w]
n0 <- length(y0)
Z <- cbind(matrix(1,n0,1),x0,(x0-c)*T0,T0)
ZZ <- solve(crossprod(Z))
beta <- ZZ%*%crossprod(Z,y0)
e <- y0 - Z%*%beta
u <- Z*(e%*%matrix(1,1,4))
V <- ZZ%*%crossprod(u)%*%ZZ
s <- sqrt(diag(V))

cat("\n")
cat("Sub-sample sizes")
print(c(n1,n2))
cat("Table 21.1 Baseline \n")
cat("Bandwidth") 
print(h)
cat("Estimated Discontinuity, s.e., t-stat, p-value \n")
print(c(theta,setheta,tstat,pvalue))
cat("\n")
cat("Simple Estimator of Equation (21.5)")
print(cbind(beta,s))

wd <- 1.4

pdf("HANSEN21-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(g1,m1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Mortality Rate",xlab="Poverty Rate",xlim=c(15,82),ylim=c(0.5,5),lwd=wd,bty="n",xaxt="n",yaxt="n")
polygon(c(g1,rev(g1)),c(L1,rev(U1)),border=NA,col=gray(.8))
polygon(c(g2,rev(g2)),c(L2,rev(U2)),border=NA,col=gray(.8))
axis(side=1,seq(10,90,10),lwd=wd)
axis(side=2,seq(0,5,1),lwd=wd)
lines(g1,m1,lwd=wd)
lines(g2,m2,lwd=wd)
abline(v=c,lwd=wd)
text(37,.75,"Untreated")
text(70,.75,"Treated")
text(66,4.7,"Cut-Off")
dev.off()

postscript("HANSEN21-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(g1,m1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Mortality Rate",xlab="Poverty Rate",xlim=c(15,82),ylim=c(0.5,5),lwd=wd,bty="n",xaxt="n",yaxt="n")
polygon(c(g1,rev(g1)),c(L1,rev(U1)),border=NA,col=gray(.8))
polygon(c(g2,rev(g2)),c(L2,rev(U2)),border=NA,col=gray(.8))
axis(side=1,seq(10,90,10),lwd=wd)
axis(side=2,seq(0,5,1),lwd=wd)
lines(g1,m1,lwd=wd)
lines(g2,m2,lwd=wd)
abline(v=c,lwd=wd)
text(37,.75,"Untreated")
text(70,.75,"Treated")
text(66,4.7,"Cut-Off")
dev.off()
