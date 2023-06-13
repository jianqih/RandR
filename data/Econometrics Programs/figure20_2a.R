#####################################
### This file generates Figure 20.2a
### Quadratic Spline Estimate of Experience Profile
### College-Educated Black Men
#####################################
### Uses data file cps09mar.txt
#####################################

dat <- read.table("cps09mar.txt")
bcollegem <- (dat[,2]==0)&(dat[,4]==16)&(dat[,11]==2)
datbm <- dat[bcollegem,]
y <- as.matrix(log(datbm[,5]/(datbm[,6]*datbm[,7])))
q <- (datbm[,1]-datbm[,4]-6)
n <- length(y)
k <- 4
p <- 5
xm <- 45;
x <- as.matrix(seq(0,xm,0.5))

xbm <- matrix(1,nrow=length(x),ncol=p+1)
zbm <- matrix(1,nrow=n,ncol=p+1)
for (j in 1:p){
  xj <- q^j
  zbj <- zbm[,1:j]
  gam <- solve(t(zbj)%*%zbj,t(zbj)%*%xj)
  egam <- xj - zbj%*%gam
  sig <- sqrt((t(egam)%*%egam)/n)
  zbm[,j+1] <- egam%*%(1/sig)
  xbm[,j+1] <- (x^j - xbm[,1:j]%*%gam)%*%(1/sig)
}

betabm <- solve(t(zbm)%*%zbm,t(zbm)%*%y)
pbm <- xbm[,1:(1+p)]%*%betabm

z <- matrix(1,nrow=n,ncol=k+2)
z[,2] <- q
xx <- matrix(1,nrow=length(x),ncol=k+2)
xx[,2] = x
for (j in 1:k){
  kj <- 10*j
  z[,j+2] <- ((q-kj)^2)*(q>kj)
  xx[,j+2] <- ((x-kj)^2)*(x>kj)
}
# betabm <- solve(t(z)%*%z,t(z)%*%y)
betabm <- qr.solve(z,y)
fbm <- xx%*%betabm
kk <- seq(21,81,20)
xk <- x[kk]
fk <- fbm[kk]

wd <- 1.4

pdf("HANSEN20-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xk,fk,xlim=c(-1,xm),ylim=c(2.4,3.5),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",pch=19,cex=.75,lwd=wd,bty="n",xaxt="n")
axis(side=2,lwd=wd)
axis(side=1,seq(-10,60,10),lwd=wd)
lines(x,fbm,lty=1,lwd=wd)
lines(x,pbm,lty=5,lwd=wd)
legend("bottomright",c("knots","quadratic spline","polynomial (6)"),lty=c(0,1,5),pch=c(19,NA,NA),pt.cex=.75,lwd=wd,bty="n")
dev.off()

postscript("HANSEN20-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xk,fk,xlim=c(-1,xm),ylim=c(2.4,3.5),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",pch=19,cex=.75,lwd=wd,bty="n",xaxt="n")
axis(side=2,lwd=wd)
axis(side=1,seq(-10,60,10),lwd=wd)
lines(x,fbm,lty=1,lwd=wd)
lines(x,pbm,lty=5,lwd=wd)
legend("bottomright",c("knots","quadratic spline","polynomial (6)"),lty=c(0,1,5),pch=c(19,NA,NA),pt.cex=.75,lwd=wd,bty="n")
dev.off()


