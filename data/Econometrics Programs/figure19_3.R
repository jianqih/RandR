#########################################################################
##  This file generates Figure 19.3
##  Boundary Bias
#########################################################################


r <- 10000
n <- 100
x <- seq(0,10,.1)
xn <- length(x)
set.seed(184)
m <- x
h1 <- 1
h2 <- 2

nw1 <- matrix(0,xn,r)
nw2 <- matrix(0,xn,r)

for (i in 1:r){

xdat <- runif(n,0,10)
ydat <-  xdat + rnorm(n)

for (j in 1:xn){
  xj <- xdat-x[j]
  k1 <- dnorm(xj/h1)
  k2 <- dnorm(xj/h2)
  nw1[j,i] <- (t(k1)%*%ydat)/sum(k1)
  nw2[j,i] <- (t(k2)%*%ydat)/sum(k2)
}
}
mnw1 <- rowMeans(nw1)
mnw2 <- rowMeans(nw2)

# Generate scatter plot for one sample
set.seed(184)
xdat <- runif(n,0,10)
ydat <-  xdat + rnorm(n)

pdf("HANSEN19-3.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,m,type="l",lty=1,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",xlim=c(0,10),ylim=c(-2,12),xaxt="n",yaxt="n",bty="n",cex.lab=.75)
points(xdat,ydat,pch=1,cex=.5)
lines(x,mnw1,lty=5)
lines(x,mnw2,lty=2)
axis(side=1,seq(0,10,1),cex.axis=.75)
axis(side=2,seq(-2,12,2),cex.axis=.75)
legend("topleft",legend=c(expression(m(x)),"Nadaraya-Watson, h=1","Nadaraya-Watson, h=2"),lty=c(1,5,2),cex=.75,bty="n")
dev.off()

postscript("HANSEN19-3.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,m,type="l",lty=1,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",xlim=c(0,10),ylim=c(-2,12),xaxt="n",yaxt="n",bty="n",cex.lab=.75)
points(xdat,ydat,pch=1,cex=.5)
lines(x,mnw1,lty=5)
lines(x,mnw2,lty=2)
axis(side=1,seq(0,10,1),cex.axis=.75)
axis(side=2,seq(-2,12,2),cex.axis=.75)
legend("topleft",legend=c(expression(m(x)),"Nadaraya-Watson, h=1","Nadaraya-Watson, h=2"),lty=c(1,5,2),cex=.75,bty="n")
dev.off()
