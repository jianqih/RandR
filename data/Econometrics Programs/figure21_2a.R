#########################################################################
##  This file generates Figure 21.2a and Table 21.1 Covariates
##  Head Start RDD with Covariates
#########################################################################
##  This file uses the package haven
##  This file uses the data file LM2007.dta
#########################################################################

library(haven)

dat <- read_dta("LM2007.dta")
y <- dat$mort_age59_related_postHS
x <- dat$povrate60
Za <- dat$census1960_pctblack
Zb <- dat$census1960_pcturban

h <- 8
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

e1 <- LL_Residual(y1,x1,h)
e2 <- LL_Residual(y2,x2,h)
e <- rbind(e1,e2)
Ra <- LL_Residual(Za,x,h)
Rb <- LL_Residual(Zb,x,h)
Z <- cbind(Za,Zb)
R <- cbind(Ra,Rb)
XXR <- solve(crossprod(R))
beta <- XXR%*%(t(R)%*%e)
u <- e - R%*%beta
Ru <- R*(u%*%matrix(1,1,ncol(R)))
V <- XXR%*%crossprod(Ru)%*%XXR
sbeta <- sqrt(diag(V))
tstat <- beta/sbeta
pvalue <- 2*(1-pnorm(abs(tstat)))
betas <- cbind(beta,sbeta,tstat,pvalue)
zm <- colMeans(Z)%*%beta

cat("Bandwidth") 
print(h)
cat("Table 21.1 Covariates \n")
cat("Linear Coefficients, s.e., t-stat, p-value \n")
print(cbind(beta,sbeta,tstat,pvalue))

Z1 <- Z[T==0,]
Z2 <- Z[T==1,]
yZ <- y - Z%*%beta
yZ1 <- y1 - Z1%*%beta
yZ2 <- y2 - Z2%*%beta

m1 <- LL_EST(yZ1,x1,g1,h) + matrix(zm,G1,1)
m2 <- LL_EST(yZ2,x2,g2,h) + matrix(zm,G2,1)
s1 <- LL_SE(yZ1,x1,g1,h)
s2 <- LL_SE(yZ2,x2,g2,h)
L1 <- m1-1.96*s1
U1 <- m1+1.96*s1
L2 <- m2-1.96*s2
U2 <- m2+1.96*s2
theta <- m2[1]-m1[G1]
setheta <- sqrt(s1[G1]^2 + s2[1]^2)
tstat <- theta/setheta
pvalue <- 2*(1-pnorm(abs(tstat)))

cat("Estimated Discontinuity, s.e., t-stat, p-value \n")
print(c(theta,setheta,tstat,pvalue))

wd <- 1.4

pdf("HANSEN21-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
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

postscript("HANSEN21-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
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


