#########################################################################
##  This file generates Figure 21.2a
##  RDD Bandwidth Selection
#########################################################################

library(haven)

dat <- read_dta("LM2007.dta")
y <- dat$mort_age59_related_postHS
x <- dat$povrate60

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

RDD_CV <- function(y1,x1,y2,x2,h) {
  e1 <- LL_Residual(y1,x1,h)
  e2 <- LL_Residual(y2,x2,h)
  cv <- (sum(e1^2)+sum(e2^2))/(length(y1)+length(y2))
  return(cv)
}

LL_ROT <- function(y,x,p) {
  n <- length(y)
  M <- max(x)-min(x)
  xs <- (x-min(x))/M
  z <- matrix(1,n,1)
  for (j in 1:p) z <- cbind(z,xs^j)
  b <- solve(crossprod(z),crossprod(z,y))
  e <- y-z%*%b
  v <- sum(e^2)/(n-ncol(z))
  s <- (1:(p-1))
  bs <- b[3:(p+1)]*s*(s+1)
  m <- z[,3:(p+1)]*bs/2
  h <- M*0.58*((v/sum(m^2))^0.2)
  return(c(h))
}

RDD_ROT <- function(y,x,T,p) {
  n <- length(y)
  M <- max(x)-min(x)
  xs <- (x-min(x))/M
  z <- matrix(1,n,1)
  for (j in 1:p) z <- cbind(z,xs^j)
  z <- cbind(z,T)
  b <- solve(crossprod(z),crossprod(z,y))
  e <- y-z%*%b
  v <- sum(e^2)/(n-ncol(z))
  s <- (1:(p-1))
  bs <- b[3:(p+1)]*s*(s+1)
  m <- z[,3:(p+1)]*bs/2
  h <- M*0.58*((v/sum(m^2))^0.2)
  return(c(h))
}

# RDD Estimation
g1 <- seq(15,59.2,.2)
g2 <- seq(59.2,82,.2)
G1 <- length(g1)
G2 <- length(g2)

h2 <- RDD_ROT(y,x,T,2)
h3 <- RDD_ROT(y,x,T,3)
h4 <- RDD_ROT(y,x,T,4)
CV <- optimize(RDD_CV,c(1,30),y1=y1,x1=x1,y2=y2,x2=x2)
hcv <- CV$minimum

cat("Bandwidths: CV, ROT(2), ROT(3), ROT(4)\n")
print(c(hcv,h2,h3,h4))


hh <- (1:30)
hg <- length(hh)
cv <- matrix(0,hg,1)
for (i in 1:hg) cv[i] <- RDD_CV(y1,x1,y2,x2,hh[i]) 

pdf("HANSEN21-cv.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(hh,cv,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xlab="",ylim=c(32.66,33.0),yaxt="n",xaxt="n")
axis(side=1,seq(0,30,5))
axis(side=2,seq(32.6,33,.05))
dev.off()
