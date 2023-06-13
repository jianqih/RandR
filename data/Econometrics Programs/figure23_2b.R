#####################################
### This file generates 
### Figure 23.2b: CES NLLS SSE contours
### CES NLLS estimation
#####################################
### This file uses the packages haven, spatstat
### This file uses the data file PSS2017.dta
#####################################

library(haven)
library(spatstat)

dat <- read_dta("PSS2017.dta")
Y <- dat$EG_total
y <- log(Y)
x1 <- dat$EC_c
x2 <- dat$EC_d
id <- dat$country
n <- length(y)

CES <- function(theta) {
  beta <- theta[1]
  rho <- theta[2]
  alpha <- theta[3]
  nu <- theta[4]
  m <- beta + (nu/rho)*log(alpha*(x1^rho)+(1-alpha)*(x2^rho))
  return(m)
}

SSE <- function(theta) mean((y - CES(theta))^2)

DM <- function(theta) {
  beta <- theta[1]
  rho <- theta[2]
  alpha <- theta[3]
  nu <- theta[4]
  D1 <- matrix(1,n,1)
  W <- alpha*(x1^rho)+(1-alpha)*(x2^rho)
  LW <- log(W)
  D2 <- -LW*(nu/(rho^2)) + (nu/rho)*(alpha*(x1^rho)*log(x1)+(1-alpha)*(x2^rho)*log(x2))/W
  D3 <- (((x1^rho)-(x2^rho))/W)*(nu/rho)
  D4 <- LW/rho
  D <- cbind(D1,D2,D3,D4)
return(D)
}

#  Estimation
theta0 <- c(1,.5,.5,1)
R <- optim(theta0,SSE)
theta <- as.matrix(R$par)
e <- y - CES(theta)
sig <- 1/(1-theta[2])
D <- DM(theta)
De <- D*rep(e,times=length(theta))
De <- rowsum(De,id)
Q <- solve(crossprod(D))
S <- crossprod(De)
V <- Q%*%S%*%Q
G <- nrow(De)
a <- G/(G-1)
se <- as.matrix(sqrt(diag(V)*a))
sesig <- se[2]*abs(sig)
thetas <- rbind(theta,sig)
ses <- rbind(se,sesig)
cat("Coefficients and se's","\n")
print(cbind(thetas,ses),digits=2)


ww <- seq(0,.7,.01)
wn <- length(ww)
pp <- c(seq(-3.05,.95,.1),1)
pn <- length(pp)
ff <- matrix(0,pn,wn)
beta <- theta[1]
rho <- theta[2]
alpha <- theta[3]
nu <- theta[4]


# Plot SSE Function as contour plot
for (i in 1:pn){
for (j in 1:wn){
  thetaf <- c(beta,pp[i],ww[j],nu)
  ff[i,j] <- SSE(thetaf)
}}

wd <- 1.4

pdf("HANSEN23-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
contour(pp,ww,ff,nlevels=25, drawlabels=FALSE,ylab=expression(alpha),xlab=expression(rho),xaxs="i",yaxs="i",xaxt="n",yaxt="n",lwd=wd,xlim=c(-3,1.01),ylim=c(0,.701),bty="n")
axis(side=1,lwd=wd)
axis(side=2,seq(0,1,.2),lwd=wd)
abline(v=1,lwd=wd)
abline(h=.7,lwd=wd)
points(rho,alpha,pch=19,col="black")
text(rho+.2,alpha+.01,expression(hat(theta)))
dev.off()

postscript("HANSEN23-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
contour(pp,ww,ff,nlevels=25, drawlabels=FALSE,ylab=expression(alpha),xlab=expression(rho),xaxs="i",yaxs="i",xaxt="n",yaxt="n",lwd=wd,xlim=c(-3,1.01),ylim=c(0,.701),bty="n")
axis(side=1,lwd=wd)
axis(side=2,seq(0,1,.2),lwd=wd)
abline(v=1,lwd=wd)
abline(h=.7,lwd=wd)
points(rho,alpha,pch=19,col="black")
text(rho+.2,alpha+.01,expression(hat(theta)))
dev.off()