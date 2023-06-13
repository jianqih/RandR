#####################################
### This file generates Figure 29.5
### Bagging
#####################################

g <- function(t,c){
 f <- t*(1-pchisq(c,3,t^2))
 return(f)
}

gg <- function(t,c,mu){ 
  f <- ((g(t,c)-mu)^2)*dnorm(t-mu)
 return(f)
}

x <- (-360:360)/100
c <- 3.84
select <- x*(x^2 > c)
bag <- g(x,c)

wd <- 1.4

pdf("HANSEN29-5a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-4,4),c(-4,4))
arrows(0,0,4,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,-4,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,4,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,-4,angle=20,length=.1,col="grey",lwd=wd)
lines(x,select,lwd=wd)
lines(x,bag,lty=2,lwd=wd)
text(-.5,3.5,expression(hat(theta)[pms]))
text(3.8,-.5,expression(hat(theta)))
text(2.5,3.3,expression(h(t)))
text(3.3,2.3,expression(g(t)))
dev.off()

postscript("HANSEN29-5a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-4,4),c(-4,4))
arrows(0,0,4,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,-4,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,4,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,-4,angle=20,length=.1,col="grey",lwd=wd)
lines(x,select,lwd=wd)
lines(x,bag,lty=2,lwd=wd)
text(-.5,3.5,expression(hat(theta)[pms]))
text(3.8,-.5,expression(hat(theta)))
text(2.5,3.3,expression(h(t)))
text(3.3,2.3,expression(g(t)))
dev.off()


x <- seq(0,5,by=0.01)
G <- length(x)
lambda <- x^2
mse1 <- 1 + (2*lambda-1)*pchisq(c,3,lambda) - lambda*pchisq(c,5,lambda)
mse2 <- matrix(0,G,1)
for (i in 1:G) {
  mi <- integrate(gg, -Inf, Inf, c=c, mu=x[i])
  mse2[i] <- mi$value
}

pdf("HANSEN29-5b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,mse1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Mean Squared Error",xlim=c(0,5),ylim=c(0,2.5),xlab=expression(theta),xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,5,1),lwd=wd)
axis(side=2,lwd=wd)
lines(x,mse2,lwd=wd)
text(3.5,2.2,expression(MSE(hat(theta)[pms])))
text(4.4,1.45,expression(MSE(hat(theta)[bag])))
dev.off()

postscript("HANSEN29-5b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,mse1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Mean Squared Error",xlim=c(0,5),ylim=c(0,2.5),xlab=expression(theta),xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,5,1),lwd=wd)
axis(side=2,lwd=wd)
lines(x,mse2,lwd=wd)
text(3.5,2.2,expression(MSE(hat(theta)[pms])))
text(4.4,1.45,expression(MSE(hat(theta)[bag])))
dev.off()


