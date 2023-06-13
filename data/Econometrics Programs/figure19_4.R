#########################################################################
##  This file generates Figure 19.4
##  Bandwidth Selection, Regression Estimation
#########################################################################


n <- 100
set.seed(180)
xm = 10
x <- runif(n,0,xm)
a <- pi/4
y <- sin((x-2)*a)/((x-2)*a) + rnorm(n)/4

# Reference Rule

z <- cbind(matrix(1,n,1),x,x^2,x^3,x^4)
beta <- solve((t(z)%*%z),(t(z)%*%y))
b <- mean((beta[3]+x*3*beta[4]+(x^2)*6*beta[5])^2)
e <- y - z%*%beta
sig <- (sum(e^2))/(n-5)
hrot <- 0.58*((xm*sig/n/b)^.2)

cat("Polynomial Regression Coefficients")
print(beta)
cat("B-hat")
print(b)
cat("sig-hat")
print(sig)
cat("ROT h")
print(hrot)

# Cross-Validation Calculation

g <- 200
h1 <- hrot/3
h2 <- 3*hrot
hh <- seq(h1,h2,(h2-h1)/g)
hn <- length(hh)

nw <- matrix(0,hn,n)
LL <- matrix(0,hn,n)
x1 <- matrix(1,n,1)
for (i in 1:hn){
hi <- hh[i]
for (j in 1:n){
  xj <- x-x[j]
  k <- dnorm(xj/hi)
  k[j] <- 0
  nw[i,j] <- (y[j]-(t(k)%*%y)/sum(k))^2
  z <- cbind(x1,xj)
  zk <- z*k
  beta <- solve(t(zk)%*%z,t(zk)%*%y)
  LL[i,j] <- (y[j]-beta[1])^2
}
}
cvnw <- rowMeans(nw)
cvLL <- rowMeans(LL)
hnw <- hh[which.min(cvnw)]
hLL <- hh[which.min(cvLL)]
nwmin <- min(cvnw)
LLmin <- min(cvLL)
cat("NW CV h")
print(hnw)
cat("LL CV h")
print(hLL)
cat("NW min CV")
print(nwmin)
cat("LL min CV")
print(LLmin)

wd <- 1.4

pdf("HANSEN19-4a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(hh,cvLL,type="l",lty=1,ylab="Cross-Validation Criterion",xlab="Bandwidth",xaxs="i",yaxs="i",ylim=c(.068,.078),lwd=wd,bty="n",yaxt="n",xaxt="n")
lines(hh,cvnw,lty=5,lwd=wd)
legend("topright",legend=c("CV(h), Nadaraya-Watson","CV(h), Local Linear"),lty=c(5,1),bg="white",lwd=wd,bty="n")
arrows(hnw,nwmin,hnw,.068,lty=5,length=.1,lwd=wd)
arrows(hLL,LLmin,hLL,.068,lty=1,length=.1,lwd=wd)
axis(1,seq(0,2,.2),lwd=wd)
axis(2,seq(.066,.078,.004),lwd=wd)
dev.off()

postscript("HANSEN19-4a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(hh,cvLL,type="l",lty=1,ylab="Cross-Validation Criterion",xlab="Bandwidth",xaxs="i",yaxs="i",ylim=c(.068,.078),lwd=wd,bty="n",yaxt="n",xaxt="n")
lines(hh,cvnw,lty=5,lwd=wd)
legend("topright",legend=c("CV(h), Nadaraya-Watson","CV(h), Local Linear"),lty=c(5,1),bg="white",lwd=wd,bty="n")
arrows(hnw,nwmin,hnw,.068,lty=5,length=.1,lwd=wd)
arrows(hLL,LLmin,hLL,.068,lty=1,length=.1,lwd=wd)
axis(1,seq(0,2,.2),lwd=wd)
axis(2,seq(.066,.078,.004),lwd=wd)
dev.off()


# Regression Estimation Using Two Bandwidths

g <- 201
xg <- seq(0,10,10/(g-1))
m <- sin((xg-2)*a)/((xg-2)*a)
m1 <- matrix(0,g,1)
m2 <- matrix(0,g,1)

for (j in 1:g){
  xj <- x-xg[j]
  z <- cbind(x1,xj)
  z1 <- z*dnorm(xj/hrot)
  z2 <- z*dnorm(xj/hLL)
  beta1 <- solve(t(z1)%*%z,t(z1)%*%y)
  beta2 <- solve(t(z2)%*%z,t(z2)%*%y)
  m1[j,1] <- beta1[1]
  m2[j,1] <- beta2[1]
}

pdf("HANSEN19-4b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xg,m1,type="l",lty=2,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",xlim=c(0,10),ylim=c(-.75,1.5),lwd=wd,bty="n",yaxt="n")
points(x,y,pch=1,cex=.7)
axis(1,lwd=wd)
axis(2,seq(-1,1.5,.5),lwd=wd)
lines(xg,m2,lty=5,lwd=wd)
lines(xg,m,lty=1,lwd=wd)
legend("topright",legend=c(expression(m(x)),expression(h[ROT]),expression(h[CV])),lty=c(1,2,5),bg="white",lwd=wd,bty="n")
dev.off()

postscript("HANSEN19-4b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE)
plot(xg,m1,type="l",lty=2,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",xlim=c(0,10),ylim=c(-.75,1.5),lwd=wd,bty="n",yaxt="n")
points(x,y,pch=1,cex=.7)
axis(1,lwd=wd)
axis(2,seq(-1,1.5,.5),lwd=wd)
lines(xg,m2,lty=5,lwd=wd)
lines(xg,m,lty=1,lwd=wd)
legend("topright",legend=c(expression(m(x)),expression(h[ROT]),expression(h[CV])),lty=c(1,2,5),bg="white",lwd=wd,bty="n")
dev.off()