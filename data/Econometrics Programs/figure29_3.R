#####################################
### This file generates Figure 29.3
### LASSO
#####################################
### Uses package spatstat
#####################################

library(spatstat)

tau = .8
x <- seq(-tau,tau,by=0.001)
ay <- tau - abs(x)
beta <- matrix(c(3/2,3/4))
phi <- -pi/8
a <- 1.44
b <- a/4
Q <- matrix(0,2,2)
cphi <- cos(phi)
sphi <- sin(phi)
Q[1,1] <- (cphi/a)^2 + (sphi/b)^2
Q[2,2] <- (sphi/a)^2 + (cphi/b)^2
Q[1,2] <- Q[2,1] <- cphi*sphi*(1/a^2 - 1/b^2)
           
G <- 200
cmax <- abs(beta[1])+abs(beta[2])
R <- matrix(1,2,1)
Qi <- solve(Q)
betas <- matrix(0,G,2)
for (i in 1:G){
  ci <- i*cmax/G
  betar <- beta - (Qi%*%R)%*%(t(R)%*%beta-ci)%*%(1/(t(R)%*%Qi%*%R))
  if (betar[1] < 0) {
    betar[1] <- 0
    betar[2] <- ci
  }
  betas[i,] <- t(betar)
}


pdf("HANSEN29-3.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-.8,3.75),c(-1.8,2.75))
polygon(c(x,rev(x)),c(ay,rev(-ay)),col=gray(.8))
arrows(-2,0,3.75,0,angle=20,length=.1,col="grey")
arrows(0,-1.05,0,2.5,angle=20,length=.1,col="grey")
text(3.5,-.2,expression(beta[1]),cex=.8)
text(.2,2.3,expression(beta[2]),cex=.8)
points(beta[1],beta[2],pch=19,col="black",cex=.6)
text(1.72,.7,"OLS",cex=.7)
W1 <- ellipse(a=a,b=b,centre=beta,phi=phi,npoly=1024)
plot(W1,add=TRUE)
W2 <- ellipse(a=a*.6,b=b*.6,centre=beta,phi=phi,npoly=1024)
plot(W2,add=TRUE)
W3 <- ellipse(a=a*1.75,b=b*1.75,centre=beta,phi=phi,npoly=1024)
plot(W3,add=TRUE)
text(1.45,-1,expression(abs(~beta[1])+abs(~beta[2])<=tau),cex=.8)
arrows(.9,-1,.4,-.4,angle=20,length=.1)
points(0,tau,pch=19,col="black",cex=.7)
text(-.7,.9,"Lasso",cex=.75)
arrows(-.425,.9,-.1,.8,angle=20,length=.1)
text(1.55,1.8,"Lasso Path",cex=.75)
arrows(1.2,1.7,1,.9,angle=20,length=.1)
lines(betas[,1],betas[,2],lty=5)
dev.off()

postscript("HANSEN29-3.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-.8,3.75),c(-1.8,2.75))
polygon(c(x,rev(x)),c(ay,rev(-ay)),col=gray(.8))
arrows(-2,0,3.75,0,angle=20,length=.1,col="grey")
arrows(0,-1.05,0,2.5,angle=20,length=.1,col="grey")
text(3.5,-.2,expression(beta[1]),cex=.8)
text(.2,2.3,expression(beta[2]),cex=.8)
points(beta[1],beta[2],pch=19,col="black",cex=.6)
text(1.72,.7,"OLS",cex=.7)
W1 <- ellipse(a=a,b=b,centre=beta,phi=phi,npoly=1024)
plot(W1,add=TRUE)
W2 <- ellipse(a=a*.6,b=b*.6,centre=beta,phi=phi,npoly=1024)
plot(W2,add=TRUE)
W3 <- ellipse(a=a*1.75,b=b*1.75,centre=beta,phi=phi,npoly=1024)
plot(W3,add=TRUE)
text(1.45,-1,expression(abs(~beta[1])+abs(~beta[2])<=tau),cex=.8)
arrows(.9,-1,.4,-.4,angle=20,length=.1)
points(0,tau,pch=19,col="black",cex=.7)
text(-.7,.9,"Lasso",cex=.75)
arrows(-.425,.9,-.1,.8,angle=20,length=.1)
text(1.55,1.8,"Lasso Path",cex=.75)
arrows(1.2,1.7,1,.9,angle=20,length=.1)
lines(betas[,1],betas[,2],lty=5)
dev.off()

