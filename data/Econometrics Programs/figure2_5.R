#################################################
### This file generates Figure 2.5
### Joint Density of CEF error e and experience 
### for white men with education = 12
#################################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
wm <- (dat[,11]==1)&(dat[,2]==0)&(dat[,4]==12)
dat1 <- dat[wm==1,]
Y <- as.matrix(log(dat1[,5]/(dat1[,6]*dat1[,7])))
X <- as.matrix(dat1[,1] - dat1[,4] - 6)
n <- length(Y)

######################################################
## CEF error 
######################################################

hx <- sd(X)/(n^(1/6))
e <- matrix(0,n,1)
for (j in 1:n){
  xj <- X-X[j]
  k <- dnorm(xj/hx)
  k[j] <- 0
  e[j,1] <- Y[j]-(t(k)%*%Y)/sum(k)
}

#######################################################
## Joint density of CEF error and experience 
#######################################################

he <- sd(e)/(n^(1/6))

## Joint density function by Gaussian Kernel
fxe <- function(x,e,error){
	out <- mean(dnorm((x-exp)/hx)*dnorm((e-error)/he))/(hx*he)
	return(out)
}

## Evaluation points
egrid <- seq(-1,1,.02)
xgrid <- seq(0,45,.50)
ne <- length(egrid)
nx <- length(xgrid)	
f <- matrix(0,nx,ne)

for(i in 1:nx){	
  xi <- xgrid[i]
  ki <- dnorm((xi-X)/hx)/hx
  for(j in 1:ne){
    ej <- egrid[j]
    kj <- dnorm((ej-e)/he)/he
    f[i,j] <- mean(ki*kj)	
  }	
}


pdf("HANSEN2-5.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)	
contour(xgrid,egrid,f,ylim=c(-1,1.2),xlim=c(0,45), zlim=c(6*10^(-3),max(f)), nlevels=7, drawlabels=FALSE,xlab="Labor Market Experience (Years)",ylab="Regression Error",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",cex.lab=.75)
axis(side=1,seq(0,45,5),cex.axis=.75)
axis(side=2,seq(-1.5,1,0.5),cex.axis=.75)
abline(h=0,lty=5)
dev.off()

postscript("HANSEN2-5.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')	
contour(xgrid,egrid,f,ylim=c(-1,1.2),xlim=c(0,45), zlim=c(6*10^(-3),max(f)), nlevels=7, drawlabels=FALSE,xlab="Labor Market Experience (Years)",ylab="Regression Error",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",cex.lab=.75)
axis(side=1,seq(0,45,5),cex.axis=.75)
axis(side=2,seq(-1.5,1,0.5),cex.axis=.75)
abline(h=0,lty=5)
dev.off()
