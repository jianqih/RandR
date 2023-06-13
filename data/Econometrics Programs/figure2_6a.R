#########################################
### This file generates Figure 2.6a
### Projection of log wage onto Education
### Linear Projection and Linear Spline 
#########################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
lnwage <- as.matrix(log(dat[,5]/(dat[,6]*dat[,7])))
wm <- (dat[,11]==1)&(dat[,2]==0)
Y <- as.matrix(lnwage[wm])
x <- dat[wm,4]

edu_dot <- c(4,6,8,9,10,11,12,13,14,16,18,20)
xfit <- seq(1,25,1)

# Conditional Mean
m <- vector()
for (i in 1:length(edu_dot)){
  m[i] <- mean(Y[x == edu_dot[i]])
}


# Linear Projection
n <- length(Y)
X <- cbind(matrix(1,n,1),x)
beta1 <- solve(t(X)%*%X)%*%(t(X)%*%Y)
f1 <- beta1[1] + xfit*beta1[2]

# Linear Spline
knot <- 9
X2 <- (x-knot)*(x>knot)
X <- cbind(X,X2)
beta2 <- solve(t(X)%*%X)%*%(t(X)%*%Y)
f2 <- beta2[1] + xfit*beta2[2] + (xfit-knot)*(xfit>knot)*beta2[3]

wd <- 1.4

pdf("HANSEN2-6a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xfit,f2, type="l" ,xlim=c(3,21),ylim=c(1.8,4),
     xlab="Education (Years)",ylab="Log Dollars per Hour",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,22,2),lwd=wd)
axis(side=2,seq(1.5,4,.5),lwd=wd)
points(edu_dot,m,cex=1.0, pch=19)
lines(xfit,f1,lty=5,lwd=wd)
legend("topleft",inset=0,legend=c("Linear Projection","Linear Spline"),lty=c(5,1),lwd=wd,bty="n")
dev.off()

postscript("HANSEN2-6a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xfit,f2, type="l" ,xlim=c(3,21),ylim=c(1.8,4),
     xlab="Education (Years)",ylab="Log Dollars per Hour",yaxt="n",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,22,2),lwd=wd)
axis(side=2,seq(1.5,4,.5),lwd=wd)
points(edu_dot,m,cex=1.0, pch=19)
lines(xfit,f1,lty=5,lwd=wd)
legend("topleft",inset=0,legend=c("Linear Projection","Linear Spline"),lty=c(5,1),lwd=wd,bty="n")
dev.off()



