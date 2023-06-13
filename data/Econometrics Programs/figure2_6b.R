#####################################
### This file generates Figure 2.6b
### Linear and quadratic projections 
### of log wage onto experience
#####################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
lnwage <- as.matrix(log(dat[,5]/(dat[,6]*dat[,7])))
wm <- (dat[,11]==1)&(dat[,2]==0)
y <- as.matrix(lnwage[wm])
x <- dat[wm,1] - dat[wm,4] - 6

xfit <- seq(0,50,.5)

# Linear Projection
n <- length(y)
x1 <- matrix(1,n,1)
X <- cbind(x1,x)
beta1 <- solve(t(X)%*%X)%*%(t(X)%*%y)
f1 <- beta1[1] + xfit*beta1[2]

# Quadratic Projection
X2 <- cbind(X,x^2)
beta2 <- solve(t(X2)%*%X2)%*%(t(X2)%*%y)
f2 <- beta2[1] + xfit*beta2[2] + (xfit^2)*beta2[3]

## Conditional Mean estimated by Local Linear Nonparametric Estimation

# Reference Rule
zz <- cbind(x1,x,x^2,x^3,x^4)
beta <- solve((t(zz)%*%zz),(t(zz)%*%y))
xtrim <- (x<=40)*(x>=0)
b <- mean(((beta[3]+x*3*beta[4]+(x^2)*6*beta[5])^2)*xtrim)
e <- y - zz%*%beta
sig <- (sum(e^2))/(n-5)
h <- 0.58*((40*sig/n/b)^.2)

# Local Linear Regression Estimation
mx <- xfit
for (j in 1:length(mx)){
  xj <- x-xfit[j]
  xx <- cbind(x1,xj)
  xh <- xx*(dnorm(xj/h)%*%cbind(1,1))
  beta <- solve(t(xh)%*%xx,t(xh)%*%y)
  mx[j] <- beta[1]
}

wd <- 1.4

leg1 <- "Conditional Expectation"
leg2 <- "Linear Projection"
leg3 <- "Quadratic Projection"

pdf("HANSEN2-6b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)	
plot(xfit,mx,xlim=c(0,50),ylim=c(1.8,4),xlab="Labor Market Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",xaxt="n",yaxt="n",bty="n",lwd=2*wd)
axis(side=1,seq(0,50,5),lwd=wd)
axis(side=2,seq(1.5,4,.5),lwd=wd)
lines(xfit,f1,lwd=wd)
lines(xfit,f2,lwd=wd)
text(22,2.3,leg1)
text(28,3.6,leg2)
text(26,2.8,leg3)
arrows(8,2.32,2.1,2.5,angle=20,length=.1,lwd=wd)
arrows(14,2.82,10.8,2.94,angle=20,length=.1,lwd=wd)
arrows(38,3.56,42,3.2,angle=20,length=.1,lwd=wd)
dev.off()

postscript("HANSEN2-6b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xfit,mx,xlim=c(0,50),ylim=c(1.8,4),xlab="Labor Market Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",xaxt="n",yaxt="n",bty="n",lwd=2*wd)
axis(side=1,seq(0,50,5),lwd=wd)
axis(side=2,seq(1.5,4,.5),lwd=wd)
lines(xfit,f1,lwd=wd)
lines(xfit,f2,lwd=wd)
text(22,2.3,leg1)
text(28,3.6,leg2)
text(26,2.8,leg3)
arrows(8,2.32,2.1,2.5,angle=20,length=.1,lwd=wd)
arrows(14,2.82,10.8,2.94,angle=20,length=.1,lwd=wd)
arrows(38,3.56,42,3.2,angle=20,length=.1,lwd=wd)
dev.off()


