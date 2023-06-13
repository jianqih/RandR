#########################################
### This file generates Figure 27.2
### Censored Regression 
#########################################

# Figure 27.2(a): Censoring Scatterplot
a <- -3
b <- 3
set.seed(10252020)
n <- 100
x <- sort(runif(n,a,b))
e <- rnorm(n)
alpha <- 1
beta <- 1
xbeta <- alpha + x*beta
y <- xbeta + e
i <- (y>0)
y0 <- y[i==0]
x0 <- x[i==0]
y1 <- y[i==1]
x1 <- x[i==1]
m1 <- xbeta*pnorm(xbeta) + dnorm(xbeta)
m2 <- xbeta + dnorm(xbeta)/pnorm(xbeta)
xb <- alpha + b*beta
xa <- alpha + a*beta
ad1 <- (xb*pnorm(xb) + dnorm(xb) - xa*pnorm(xa) + dnorm(xa))/(b-a)
maxy <- max(y)
miny <- min(y)

wd <- 1.4

pdf("HANSEN27-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x1,y1,ylab="Dependent Variable",xlab="Regressor",yaxt="n",xaxt="n",bty="n",xlim=c(a,b),ylim=c(miny,maxy),pch=0,cex=.6)
axis(side=1,seq(-4,4,1),lwd=wd)
axis(side=2,seq(-6,6,2),lwd=wd)
points(x0,y0,pch=1,cex=.6)
points(x0,matrix(0,length(x0),1),pch=19,cex=.6)
abline(h=0,lwd=wd,lty=3)
lines(c(a,b),c(alpha+a*beta,alpha+b*beta),lwd=wd)
lines(x,m1,lty=2,lwd=wd)
lines(x,m2,lty=5,lwd=wd)
legend("bottomright",c(expression(paste(m^'#',(x))),"m(x)","m*(x)","Y>0 (uncensored)","Y<0 (unobserved)","Y=0 (censored)"),lty=c(5,2,1,0,0,0),pch=c(NA,NA,NA,0,1,19),pt.cex=.6,cex=.785,bty="n",lwd=wd)
dev.off()


postscript("HANSEN27-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x1,y1,ylab="Dependent Variable",xlab="Regressor",yaxt="n",xaxt="n",bty="n",xlim=c(a,b),ylim=c(miny,maxy),pch=0,cex=.6)
axis(side=1,seq(-4,4,1),lwd=wd)
axis(side=2,seq(-6,6,2),lwd=wd)
points(x0,y0,pch=1,cex=.6)
points(x0,matrix(0,length(x0),1),pch=19,cex=.6)
abline(h=0,lwd=wd,lty=3)
lines(c(a,b),c(alpha+a*beta,alpha+b*beta),lwd=wd)
lines(x,m1,lty=2,lwd=wd)
lines(x,m2,lty=5,lwd=wd)
legend("bottomright",c(expression(paste(m^'#',(x))),"m(x)","m*(x)","Y>0 (uncensored)","Y<0 (unobserved)","Y=0 (censored)"),lty=c(5,2,1,0,0,0),pch=c(NA,NA,NA,0,1,19),pt.cex=.6,cex=.785,bty="n",lwd=wd)
dev.off()



# Figure 27.2(b): Probability of Censoring
xx <- seq(a,b,.1)
yy <- pnorm(-alpha-xx*beta)

pdf("HANSEN27-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xx,yy,type="l",xlab="Regressor",ylab="Probability",yaxt="n",xaxt="n",bty="n",xlim=c(a,b),ylim=c(0,1),lwd=wd)
axis(side=1,seq(-4,4,1),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
text(-.5,.7,"P[Y=0|X]")
dev.off()

postscript("HANSEN27-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xx,yy,type="l",xlab="Regressor",ylab="Probability",yaxt="n",xaxt="n",bty="n",xlim=c(a,b),ylim=c(0,1),lwd=wd)
axis(side=1,seq(-4,4,1),lwd=wd)
axis(side=2,seq(-1,2,.2),lwd=wd)
text(-.5,.7,"P[Y=0|X]")
dev.off()



