#####################################
### This file generates Figure 20.3
### Polynomials as Kernels
#####################################

g <- 2001
x <- as.matrix(seq(0,1,1/(g-1)))
p1 <- 4
p2 <- 8
p3 <- 12
xa <- .5
xb <- .25

K <- p3+1
Q <- matrix(0,K,K)
for (j in 1:K){
for (k in 1:K){
  Q[j,k] <- 1/(j+k-1)
}}

Z <- matrix(1,g,K)
for (j in 1:p3){
  Z[,j+1] <- x^j
}

Z1 <- Z[,1:p1] 
Z2 <- Z[,1:p2] 
Z3 <- Z[,1:p3] 

qa <- round((g-1)*xa)+1
qb <- round((g-1)*xb)+1

Q1 <- solve(Q[1:p1,1:p1],tol=1e-20)
Q2 <- solve(Q[1:p2,1:p2],tol=1e-20)
Q3 <- solve(Q[1:p3,1:p3],tol=1e-20)

w1a <- Z1%*%Q1%*%Z1[qa,]
w2a <- Z2%*%Q2%*%Z2[qa,]
w3a <- Z3%*%Q3%*%Z3[qa,]

w1b <- Z1%*%Q1%*%Z1[qb,]
w2b <- Z2%*%Q2%*%Z2[qb,]
w3b <- Z3%*%Q3%*%Z3[qb,]

z0 <- matrix(0,g,1)

wd <- 1.4

pdf("HANSEN20-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,w3a,lty=1,xaxs="i",yaxs="i",type="l",ylab="Kernel Function",xlab="",ylim=c(-3,8),lwd=wd,bty="n")
axis(side=1,lwd=wd)
axis(side=2,seq(-4,8,2),lwd=wd)
lines(x,w2a,lty=2,lwd=wd)
lines(x,w1a,lty=5,lwd=wd)
lines(x,z0,lty=3,lwd=wd)
arrows(xa, w3a[qa],xa,-3,code=2,length=.1,lwd=wd)
mtext(expression(x),side=1,at=xa)
legend("topright",c(expression(p==4),expression(p==8),expression(p==12)),lty=c(5,2,1),lwd=wd,bty="n")
dev.off()

postscript("HANSEN20-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,w3a,lty=1,xaxs="i",yaxs="i",type="l",ylab="Kernel Function",xlab="",ylim=c(-3,8),lwd=wd,bty="n")
axis(side=1,lwd=wd)
axis(side=2,seq(-4,8,2),lwd=wd)
lines(x,w2a,lty=2,lwd=wd)
lines(x,w1a,lty=5,lwd=wd)
lines(x,z0,lty=3,lwd=wd)
arrows(xa, w3a[qa],xa,-3,code=2,length=.1,lwd=wd)
mtext(expression(x),side=1,at=xa)
legend("topright",c(expression(p==4),expression(p==8),expression(p==12)),lty=c(5,2,1),lwd=wd,bty="n")
dev.off()

pdf("HANSEN20-3b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,w3b,lty=1,xaxs="i",yaxs="i",type="l",ylab="Kernel Function",xlab="",ylim=c(-4,9),lwd=wd,bty="n")
axis(side=1,lwd=wd)
axis(side=2,lwd=wd)
lines(x,w1b,lty=5,lwd=wd)
lines(x,z0,lty=3,lwd=wd)
arrows(xb, w3b[qb],xb,-4,code=2,length=.1,lwd=wd)
mtext(expression(x),side=1,at=xb)
legend("topright",c(expression(p==4),expression(p==12)),lty=c(5,1),lwd=wd,bty="n")
dev.off()

postscript("HANSEN20-3b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,w3b,lty=1,xaxs="i",yaxs="i",type="l",ylab="Kernel Function",xlab="",ylim=c(-4,9),lwd=wd,bty="n")
axis(side=1,lwd=wd)
axis(side=2,lwd=wd)
lines(x,w1b,lty=5,lwd=wd)
lines(x,z0,lty=3,lwd=wd)
arrows(xb, w3b[qb],xb,-4,code=2,length=.1,lwd=wd)
mtext(expression(x),side=1,at=xb)
legend("topright",c(expression(p==4),expression(p==12)),lty=c(5,1),lwd=wd,bty="n")
dev.off()

