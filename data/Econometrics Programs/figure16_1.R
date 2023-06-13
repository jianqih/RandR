#####################################
### This file generates Figures 16.1 and 16.2
### Unit Root Processes
#####################################


set.seed(6112020)
n <- 240
y <- as.matrix(cumsum(rnorm(n-1)))
y <- rbind(0,y)
x <- seq(1:n)

ybar <- mean(y)
m <- matrix(ybar,n,1)
n2 <- n/2
ybar1 <- mean(y[1:n2])
x1 <- x[1:n2]
m1 <- matrix(ybar1,n2,1)
ybar2 <- mean(y[(n2+1):n])
x2 <- x[(n2+1):n]
m2 <- matrix(ybar2,n2,1)
X <- cbind(matrix(1,n,1),x)
beta <- solve(crossprod(X),crossprod(X,y))
trend <- X%*%beta

e1 <- y - m
e2 <- y - trend
e3 <- y - x*(y[n]/n)

wd <- 1.4

pdf("HANSEN16-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,y,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(0,240),lwd=wd) 
lines(x,m,,lty=5,lwd=wd)
axis(side=1,seq(0,240,40),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
text(40,-6.5,"Sample Mean")
arrows(40,-5.6,40,-3.2,code=2,length=0.15,lwd=wd)
lines(x1,m1,lty=2,lwd=wd)
lines(x2,m2,lty=2,lwd=wd)
text(105,2.5,expression(paste("Mean, ",t<=120)))
text(202,2.5,expression(paste("Mean, ",t>120)))
arrows(95,1.5,95,-.7,code=2,length=0.15,lwd=wd)
arrows(215,1.5,215,-5,code=2,length=0.15,lwd=wd)
dev.off()

postscript("HANSEN16-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,y,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(0,240),lwd=wd) 
lines(x,m,,lty=5,lwd=wd)
axis(side=1,seq(0,240,40),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
text(40,-6.5,"Sample Mean")
arrows(40,-5.6,40,-3.2,code=2,length=0.15,lwd=wd)
lines(x1,m1,lty=2,lwd=wd)
lines(x2,m2,lty=2,lwd=wd)
text(105,2.5,expression(paste("Mean, ",t<=120)))
text(202,2.5,expression(paste("Mean, ",t>120)))
arrows(95,1.5,95,-.7,code=2,length=0.15,lwd=wd)
arrows(215,1.5,215,-5,code=2,length=0.15,lwd=wd)
dev.off()

pdf("HANSEN16-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,y,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(0,240),lwd=wd) 
lines(x,trend,lty=5,lwd=wd)
axis(side=1,seq(0,240,40),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
text(100,2.5,"Fitted Trend")
arrows(100,1.5,100,-2,code=2,length=0.15,lwd=wd)
dev.off()

postscript("HANSEN16-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,y,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(0,240),lwd=wd) 
lines(x,trend,lty=5,lwd=wd)
axis(side=1,seq(0,240,40),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
text(100,2,"Fitted Trend",cex=.8)
arrows(100,1.5,100,-2,code=2,length=0.15,lwd=wd)
dev.off()

Z <- matrix(0,n,1)

pdf("HANSEN16-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,e2,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(0,240),ylim=c(-6,10),lwd=wd) 
axis(side=1,seq(0,240,40),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
lines(x,Z,lty=2,lwd=wd)
dev.off()

postscript("HANSEN16-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,e2,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(0,240),ylim=c(-6,10),lwd=wd) 
axis(side=1,seq(0,240,40),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
lines(x,Z,lty=2,lwd=wd)
dev.off()

pdf("HANSEN16-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,e3,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(0,240),ylim=c(-6,10),lwd=wd) 
axis(side=1,seq(0,240,40),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
lines(x,Z,lty=2,lwd=wd)
dev.off()

postscript("HANSEN16-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,e3,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",xlim=c(0,240),ylim=c(-6,10),lwd=wd) 
axis(side=1,seq(0,240,40),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
lines(x,Z,lty=2,lwd=wd)
dev.off()
