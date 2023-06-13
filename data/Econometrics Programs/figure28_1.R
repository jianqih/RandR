#####################################
### This file generates Figure 28.1ab
### MSE of PMS estimators
#####################################


K <- 1
x <- seq(0,5,by=0.01)
lambda <- x^2
G <- length(x)
y1 <- matrix(1,G,1)
c <- 2*K
y2 <- K + (lambda*2-K)*pchisq(c,K+2,lambda) - lambda*pchisq(c,K+4,lambda)
c <- qchisq(.95,K)
y3 <- K + (lambda*2-K)*pchisq(c,K+2,lambda) - lambda*pchisq(c,K+4,lambda)
c <- K*log(200)
y4 <- K + (lambda*2-K)*pchisq(c,K+2,lambda) - lambda*pchisq(c,K+4,lambda)
c <- K*log(1000)
y5 <- K + (lambda*2-K)*pchisq(c,K+2,lambda) - lambda*pchisq(c,K+4,lambda)

wd <- 1.4

pdf("HANSEN28-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,y1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Mean Squared Error",xlim=c(0,5),ylim=c(0,4),xlab=expression(root(lambda)),xaxt="n",bty="n")
axis(side=1,seq(0,5,1),lwd=wd)
lines(x,y2,lty=1,lwd=wd)
lines(x,y3,lty=1,lwd=wd)
lines(x,y4,lty=1,lwd=wd)
lines(x,y5,lty=1,lwd=wd)
text(3.8,3.8,"BIC, n=1000")
text(4,3.4,"BIC, n=200")
text(4.2,3,"5% Sig Test")
text(4.2,2.6,"AIC")
text(2,.8,"No Selection")
arrows(3.35,3.35,3,2.8,angle=20,length=.1,lwd=wd)
arrows(3.52,2.93,3,2.0,angle=20,length=.1,lwd=wd)
arrows(3.95,2.5,3.2,1.2,angle=20,length=.1,lwd=wd)
dev.off()

postscript("HANSEN28-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,y1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Mean Squared Error",xlim=c(0,5),ylim=c(0,4),xlab=expression(root(lambda)),xaxt="n",bty="n")
axis(side=1,seq(0,5,1),lwd=wd)
lines(x,y2,lty=1,lwd=wd)
lines(x,y3,lty=1,lwd=wd)
lines(x,y4,lty=1,lwd=wd)
lines(x,y5,lty=1,lwd=wd)
text(3.8,3.8,"BIC, n=1000")
text(4,3.4,"BIC, n=200")
text(4.2,3,"5% Sig Test")
text(4.2,2.6,"AIC")
text(2,.8,"No Selection")
arrows(3.35,3.35,3,2.8,angle=20,length=.1,lwd=wd)
arrows(3.52,2.93,3,2.0,angle=20,length=.1,lwd=wd)
arrows(3.95,2.5,3.2,1.2,angle=20,length=.1,lwd=wd)
dev.off()


K <- 5
x <- seq(0,8,by=.1)
lambda <- x^2
G <- length(x)
y1 <- matrix(K,G,1)
c <- 2*K
y2 <- K + (lambda*2-K)*pchisq(c,K+2,lambda) - lambda*pchisq(c,K+4,lambda)
c <- qchisq(.95,K)
y3 <- K + (lambda*2-K)*pchisq(c,K+2,lambda) - lambda*pchisq(c,K+4,lambda)
c <- K*log(200)
y4 <- K + (lambda*2-K)*pchisq(c,K+2,lambda) - lambda*pchisq(c,K+4,lambda)
c <- K*log(1000)
y5 <- K + (lambda*2-K)*pchisq(c,K+2,lambda) - lambda*pchisq(c,K+4,lambda)

pdf("HANSEN28-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,y1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Mean Squared Error",xlim=c(0,8),ylim=c(0,20),xlab=expression(root(lambda)),xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,10,1),lwd=wd)
lines(x,y2,lty=1,lwd=wd)
lines(x,y3,lty=1,lwd=wd)
lines(x,y4,lty=1,lwd=wd)
lines(x,y5,lty=1,lwd=wd)
text(6.7,19,"BIC, n=1000")
text(7,16,"BIC, n=200")
text(4.8,7,"5% Sig Test")
text(2,9,"AIC")
text(3.5,4,"No Selection")
arrows(5.95,16,5.15,13,angle=20,length=.1,lwd=wd)
arrows(2.3,8.5,2.7,6.5,angle=20,length=.1,lwd=wd)
dev.off()

postscript("HANSEN28-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,y1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Mean Squared Error",xlim=c(0,8),ylim=c(0,20),xlab=expression(root(lambda)),xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,10,1),lwd=wd)
lines(x,y2,lty=1,lwd=wd)
lines(x,y3,lty=1,lwd=wd)
lines(x,y4,lty=1,lwd=wd)
lines(x,y5,lty=1,lwd=wd)
text(6.7,19,"BIC, n=1000")
text(7,16,"BIC, n=200")
text(4.8,7,"5% Sig Test")
text(2,9,"AIC")
text(3.5,4,"No Selection")
arrows(5.95,16,5.15,13,angle=20,length=.1,lwd=wd)
arrows(2.3,8.5,2.7,6.5,angle=20,length=.1,lwd=wd)
dev.off()


