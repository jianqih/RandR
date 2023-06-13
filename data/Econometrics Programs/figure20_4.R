#####################################
### This file generates Figure 20.4
### Length of Regressor Vector
#####################################

p <- 9
K <- p+1
q <- matrix(0,K,K)
g <- 101
x <- as.matrix(seq(0,1,1/(g-1)))

q1 <- matrix(0,K,K)
for (j in 1:K){
for (k in 1:K){
  q1[j,k] <- 1/(j+k-1)
}}
q1i <- solve(q1,tol=1e-16)
Z1 <- matrix(0,g,1)
for (j in 1:g){
  xj <- x[j]^(0:p)
  Z1[j] <- sqrt(t(xj)%*%q1i%*%xj)
}

s <- 2
G <- 1000
q2 <- matrix(0,K,K)
kn <- p-s
knots <- (1:kn)/(kn+1)
for (j in 1:G){
  xj <- (j/G)
  xxj <- matrix(1,K,1)
  xxj[2:(s+1)] <- xj^(1:s)
  xxj[(s+2):K] <- ((xj - knots)^s)*(xj > knots)
  R <- xxj%*%t(xxj)
  q2 <- q2 + R/G
}
q2i <- solve(q2,tol=1e-16)
Z2 <- matrix(0,g,1)
for (j in 1:g){
  xj <- x[j]
  xxj <- matrix(1,K,1)
  xxj[2:(s+1)] <- xj^(1:s)
  xxj[(s+2):K] <- ((xj - knots)^s)*(xj > knots)
  Z2[j] <- sqrt(t(xxj)%*%q2i%*%xxj)
}

pdf("HANSEN20-4.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,Z1,lty=1,xaxs="i",yaxs="i",type="l",ylab="Length of Regressor Vector",xlab="x",ylim=c(0,K),cex.lab=.75,bty="n",xaxt="n",yaxt="n")
axis(side=1,cex.axis=.75)
axis(side=2,cex.axis=.75)
lines(x,Z2,lty=5)
legend("top",c("Polynomial","Quadratic Spline"),lty=c(1,5),cex=.75,bty="n")
dev.off()

postscript("HANSEN20-4.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,Z1,lty=1,xaxs="i",yaxs="i",type="l",ylab="Length of Regressor Vector",xlab="x",ylim=c(0,K),cex.lab=.75,bty="n",xaxt="n",yaxt="n")
axis(side=1,cex.axis=.75)
axis(side=2,cex.axis=.75)
lines(x,Z2,lty=5)
legend("top",c("Polynomial","Quadratic Spline"),lty=c(1,5),cex=.75,bty="n")
dev.off()

