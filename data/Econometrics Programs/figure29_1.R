#####################################
### This file generates Figure 29.1
### Ridge Regression
#####################################
### Uses package spatstat
#####################################

library(spatstat)

x <- seq(-1,1,by=0.001)
y <- sqrt(1 - x^2)
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

f <- function(x,c) t(beta)%*%Q%*%solve(Q+diag(2)*x)%*%solve(Q+diag(2)*x)%*%Q%*%beta-c
ridge <- uniroot(f,c(0,100),c=1)
lambda <- ridge$root
bridge <- solve(Q+diag(2)*lambda)%*%Q%*%beta

G <- 200
cmax <- t(beta)%*%beta
betas <- matrix(0,G,2)
for (i in 1:G){
  ci <- i*cmax/G
  ridge <- uniroot(f,c(0,1000),c=ci)
  lambdai <- ridge$root
  bi <- solve(Q+diag(2)*lambdai)%*%Q%*%beta
  betas[i,] <- t(bi)
}


pdf("HANSEN29-1.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-1,3.75),c(-2,2.75))
polygon(c(x,rev(x)),c(y,rev(-y)),col=gray(.8))
arrows(-2,0,3.75,0,angle=20,length=.1,col="grey")
arrows(0,-1.25,0,2.5,angle=20,length=.1,col="grey")
text(3.75,-.25,expression(beta[1]),cex=.8)
text(.2,2.3,expression(beta[2]),cex=.8)
points(beta[1],beta[2],pch=19,col="black",cex=.6)
text(1.72,.7,"OLS",cex=.7)
W1 <- ellipse(a=a,b=b,centre=beta,phi=phi,npoly=1024)
plot(W1,add=TRUE)
W2 <- ellipse(a=a/2,b=b/2,centre=beta,phi=phi,npoly=1024)
plot(W2,add=TRUE)
W3 <- ellipse(a=a*3/4,b=b*3/4,centre=beta,phi=phi,npoly=1024)
plot(W3,add=TRUE)
points(bridge[1],bridge[2],pch=19,col="black",cex=.7)
text(.6,.3,"Ridge",cex=.75)
arrows(.475,.4,.53,.775,angle=20,length=.1)
text(1.5,-1,expression(beta[1]^2+beta[2]^2<=tau),cex=.8)
arrows(1,-1,.7,-.7,angle=20,length=.1)
text(1.5,1.8,"Ridge Path",cex=.75)
arrows(1.2,1.7,1,.9,angle=20,length=.1)
points(0,0,pch=19,col="black",cex=.7)
lines(betas[,1],betas[,2],lty=5)
dev.off()

postscript("HANSEN29-1.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-1,3.75),c(-2,2.75))
polygon(c(x,rev(x)),c(y,rev(-y)),col=gray(.8))
arrows(-2,0,3.75,0,angle=20,length=.1,col="grey")
arrows(0,-1.25,0,2.5,angle=20,length=.1,col="grey")
text(3.75,-.25,expression(beta[1]),cex=.8)
text(.2,2.3,expression(beta[2]),cex=.8)
points(beta[1],beta[2],pch=19,col="black",cex=.6)
text(1.72,.7,"OLS",cex=.7)
W1 <- ellipse(a=a,b=b,centre=beta,phi=phi,npoly=1024)
plot(W1,add=TRUE)
W2 <- ellipse(a=a/2,b=b/2,centre=beta,phi=phi,npoly=1024)
plot(W2,add=TRUE)
W3 <- ellipse(a=a*3/4,b=b*3/4,centre=beta,phi=phi,npoly=1024)
plot(W3,add=TRUE)
points(bridge[1],bridge[2],pch=19,col="black",cex=.7)
text(.6,.3,"Ridge",cex=.75)
arrows(.475,.4,.53,.775,angle=20,length=.1)
text(1.5,-1,expression(beta[1]^2+beta[2]^2<=tau),cex=.8)
arrows(1,-1,.7,-.7,angle=20,length=.1)
text(1.5,1.8,"Ridge Path",cex=.75)
arrows(1.2,1.7,1,.9,angle=20,length=.1)
points(0,0,pch=19,col="black",cex=.7)
lines(betas[,1],betas[,2],lty=5)
dev.off()

