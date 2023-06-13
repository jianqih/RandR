#####################################
### This file generates Figure 28.4b
### Investment Firm Effects
#####################################
### Uses package haven
### Uses data file Invest1993.dta
#####################################

library(haven)

invest <- read_dta("Invest1993.dta")

invest <- invest[invest$nyseamex==1,]
invest <- invest[invest$year>=1982,]
invest <- invest[invest$year<=1991,]
m <- rowsum(matrix(1,nrow(invest),1),invest$cusip)
invest <- invest[rep(m,m)>1,]

attach(invest)

firm <- cusip
industry <- ardsic
Year <- year
x1 <- cbind(vala,debta,cfa)
y <- inva

detach(invest)

n <- length(firm)
id <- unique(firm)
sic <- unique(industry)
years <- as.matrix(1983:1990)
yr <- (Year%*%matrix(1,1,length(years)) == matrix(1,n,1)%*%t(years))
x <- cbind(x1,yr)
m <- rowsum(matrix(1,n,1),firm)

N <- length(id)
Kx <- ncol(x)
K  <- Kx + N
xx <- matrix(0,Kx,Kx)
xy <- matrix(0,Kx,1)
yy <- 0
ybar <- matrix(0,N,1)
xbar <- matrix(0,N,Kx)

bandwidth <- .007
# Note: Bandwidth (for density estimation) selected by comparing ROT and SJ bandwidths for both estimators

for (i in 1:N) {
 Si <- (firm == id[i])
 xi <- x[Si,]
 yi <- y[Si]
 ni <- nrow(xi)-1
 xi <- xi[1:ni,]
 yi <- yi[2:(ni+1)]
 yb <- mean(yi)
 if (ni==1) { xb <- xi } 
 else { xb <- colMeans(xi) }
 xib <- xi - matrix(1,ni,1)%*%xb
 yib <- yi - yb
 ybar[i]  <- yb
 xbar[i,] <- xb
 xx <- xx + t(xib)%*%xib
 xy <- xy + t(xib)%*%yib
 yy <- yy + t(yib)%*%yib
}
beta <- solve(xx,xy)
alpha <- ybar - xbar%*%beta
xxi <- solve(xx)
sig <- (yy - t(xy)%*%beta)/(n-N)
sigs <- (yy - t(xy)%*%beta)/(n-N-K)
den <- density(alpha,bw=bandwidth)

Ks <- length(sic)
ss <- (industry%*%matrix(1,1,Ks) == matrix(1,n,1)%*%t(sic))
x <- cbind(x,ss)
xxi <- solve(t(x)%*%x)
betaC <- xxi%*%(t(x)%*%y)
e <- y - x%*%betaC
sigC <- mean(e^2)
J <- (n-N-K)*(sigC-sig)/sigs
sics <- rowsum(industry,firm)/m
SS <- (sics%*%matrix(1,1,Ks) == matrix(1,N,1)%*%t(sic))
alphaf <- SS%*%betaC[(Kx+1):(Kx+Ks)]
da <- alpha-alphaf 
alphaJS <- alpha - da%*%((N-Ks-2)/J)
denJS <- density(alphaJS,bw=bandwidth)

wd <- 1.4

pdf("HANSEN28-4b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(denJS$x, denJS$y, type="l",lty=1,title=NULL,xaxs="i",yaxs="i",xlim=c(-.05,.2),ylab="",xlab="Density of Individual Firm Effect",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-.05,.2,.05),lwd=wd)
lines(den$x,den$y,lty=1,lwd=wd)
text(.11,14,"James-Stein")
text(.165,2.5,"Fixed Effects")
dev.off()

postscript("HANSEN28-4b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(denJS$x, denJS$y, type="l",lty=1,title=NULL,xaxs="i",yaxs="i",xlim=c(-.05,.2),ylab="",xlab="Density of Individual Firm Effect",xaxt="n",yaxt="n",bty="n",lwd=wd)
axis(side=1,seq(-.05,.2,.05),lwd=wd)
lines(den$x,den$y,lty=1,lwd=wd)
text(.11,14,"James-Stein")
text(.165,2.5,"Fixed Effects")
dev.off()


