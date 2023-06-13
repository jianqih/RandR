######################################################################
### This file generates Figure 2.4a
### Joint density of log wage and experience,
### Conditional mean of log wage given experience,
### for White men with 12-years education
######################################################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
wm <- (dat[,11]==1)&(dat[,2]==0)&(dat[,4]==12)
dat1 <- dat[wm,]
y <- as.matrix(log(dat1[,5]/(dat1[,6]*dat1[,7])))
x <- as.matrix(dat1[,1]-dat1[,4]-6)
n <- length(y)

#######################################################
## Joint PDF of LogWage and experience
#######################################################

## bandwidths
hx <- sd(x)/(n^(1/6))
hy <- sd(y)/(n^(1/6))

## evaluation region 
xg <- seq(0,47,.5)
yg <- seq(1.8,3.9,0.05)	
nx <- length(xg)
ny <- length(yg)
fjoint <- matrix(0,nx,ny)

## Joint density
for(i in 1:length(xg)){	
  fi <- dnorm(x - xg[i],sd=hx)
  for(j in 1:length(yg)){
    fj <- dnorm(y - yg[j],sd=hy)
    fjoint[i,j] <- mean(fi*fj)		
  }	
}

########################################################################
## Conditional Mean estimated by Local Linear Nonparametric Estimation
########################################################################

# Reference Rule
x1 <- matrix(1,n,1)
zz <- cbind(x1,x,x^2,x^3,x^4)
beta <- solve((t(zz)%*%zz),(t(zz)%*%y))
xtrim <- (x<=40)*(x>=0)
b <- mean(((beta[3]+x*3*beta[4]+(x^2)*6*beta[5])^2)*xtrim)
e <- y - zz%*%beta
sig <- (sum(e^2))/(n-5)
h <- 0.58*((40*sig/n/b)^.2)

# Local Linear Regression Estimation
mx <- xg
for (j in 1:length(mx)){
  xj <- x-xg[j]
  xx <- cbind(x1,xj)
  xh <- xx*(dnorm(xj/h)%*%cbind(1,1))
  beta <- solve(t(xh)%*%xx,t(xh)%*%y)
  mx[j] <- beta[1]
}

wd <- 1.4

pdf("HANSEN2-4a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)	
contour(xg,yg,fjoint,ylim=c(1.8,3.9),xlim=c(0,47),zlim=c(5*10^(-3),max(fjoint)),nlevels=7,drawlabels=FALSE,xlab="Labor Market Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",yaxt="n",xaxt="n",bty="n",lwd=wd)	
axis(side=1,seq(0,50,5),lwd=wd)
axis(side=2,seq(1.5,4.5,.5),lwd=wd)
lines(xg,mx,lwd=wd)
legend("bottomright","Conditional Expectation",lty=1,cex=.8,bty="n",lwd=wd)
dev.off()

postscript("HANSEN2-4a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
contour(xg,yg,fjoint,ylim=c(1.8,3.9),xlim=c(0,47),zlim=c(5*10^(-3),max(fjoint)),nlevels=7,drawlabels=FALSE,xlab="Labor Market Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",yaxt="n",xaxt="n",bty="n",lwd=wd)	
axis(side=1,seq(0,50,5),lwd=wd)
axis(side=2,seq(1.5,4.5,.5),lwd=wd)
lines(xg,mx,lwd=wd)
legend("bottomright","Conditional Expectation",lty=1,cex=.8,bty="n",lwd=wd)
dev.off()
