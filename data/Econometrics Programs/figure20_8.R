#####################################
### This file generates Figure 20.8
### 
#####################################
### Uses package haven
### Uses data file cps09mar.txt
#####################################

library(haven)

data <- read_dta("AL1999.dta")
avgverb <- as.matrix(data$avgverb)
avgmath <- as.matrix(data$avgmath)
enrollment <- as.matrix(data$enrollment)
schlcode <- as.matrix(data$schlcode)
disadvantaged <- as.matrix(data$disadvantaged)
classize <- as.matrix(data$classize)
grade <- as.matrix(data$grade)
fourth <- as.matrix(grade == 4)
n <- nrow(avgverb)
pclass <- (enrollment/floor(1+(enrollment-1)/40))
y <- avgverb

c <- classize/40
pc <- pclass/40
d <- disadvantaged/14

pmax <- 5
D <- matrix(1,n,pmax)
C <- matrix(1,n,pmax)
Z <- matrix(1,n,pmax)
for (j in 1:pmax){
  D[,j] <- d^j
  C[,j] <- c^j
  Z[,j] <- pc^j
}
x0 <- cbind(enrollment,fourth,matrix(1,n,1))

CV <- matrix(0,n,pmax)
for (i in 1:n) {
for (j in 1:pmax){
  xj <- cbind(D[,1:j],x0)
  k <- (schlcode != schlcode[i])
  xjk <- xj*(k%*%matrix(1,1,ncol(xj)))
  CV[i,j] <- y[i] - xj[i,]%*%solve(t(xjk)%*%xjk)%*%(t(xjk)%*%y)
}}
CVs = colMeans(CV^2)
j <- which.min(CVs)
print(j)
K <- seq(1,pmax,1)

pdf("HANSEN20-8cv.pdf")
plot(K,CVs,lty=1,lwd=wd,xaxs="i",yaxs="i",type="l",xlab="K",ylab="CV(K)")
dev.off()

x <- cbind(C[,1:3],C[,1]*D[,1],D[,1:3],x0)
z <- cbind(Z[,1:3],Z[,1]*D[,1],D[,1:3],x0)
zx <- solve(t(z)%*%x)
b <- zx%*%(t(z)%*%y)
e <- matrix(0,n,1)
for (i in 1:n) {
  k <- (schlcode != schlcode[i])
  zk <- z*(k%*%matrix(1,1,ncol(z)))
  e[i] <- y[i] - x[i,]%*%solve(t(zk)%*%x)%*%(t(zk)%*%y)
}
ze <- z*rep(e,times=ncol(x))
ze_sum <- rowsum(ze,schlcode)
G <- nrow(ze_sum)
v <- (G/(G-1))*zx%*%(t(ze_sum)%*%ze_sum)%*%t(zx)

x1p <- seq(20,40,.1)
n1 <- length(x1p)
d1 <- mean(d)
D1 <- matrix(1,n1,1)%*%cbind(d1,d1^2,d1^3)
X0 <- matrix(1,n1,1)%*%colMeans(x0)
X1 <- cbind((x1p/40),(x1p/40)^2,(x1p/40)^3,(x1p/40)*D1[,1],D1,X0)
f1 <- X1%*%b
se1 <- sqrt(rowSums(X1*(X1%*%v)))
f1L <- f1 - se1*1.96
f1U <- f1 + se1*1.96

wd <- 1.4

pdf("HANSEN20-8a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x1p,f1,lty=1,xaxs="i",yaxs="i",type="l",xlab="Class Size",ylab="Reading Test Score",ylim=c(62,82),lwd=wd,bty="n",yaxt="n")
polygon(c(x1p,rev(x1p)),c(f1U,rev(f1L)),col=gray(.8),border=NA)
axis(side=1,lwd=wd)
axis(side=2,seq(60,85,5),lwd=wd)
lines(x1p,f1,lwd=wd)
dev.off()

postscript("HANSEN20-8a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x1p,f1,lty=1,xaxs="i",yaxs="i",type="l",xlab="Class Size",ylab="Reading Test Score",ylim=c(62,82),lwd=wd,bty="n",yaxt="n")
polygon(c(x1p,rev(x1p)),c(f1U,rev(f1L)),col=gray(.8),border=NA)
axis(side=1,lwd=wd)
axis(side=2,seq(60,85,5),lwd=wd)
lines(x1p,f1,lwd=wd)
dev.off()


x2p <- seq(0,50,.1)
n2 <- length(x2p)
c1 <- mean(c)
C1 <- matrix(1,n2,1)%*%cbind(c1,c1^2,c1^3)
X0 <- matrix(1,n2,1)%*%colMeans(x0)
X2 <- cbind(C1,C1[,1]*(x2p/14),(x2p/14),(x2p/14)^2,(x2p/14 )^3,X0)
f2 <- X2%*%b
se2 <- sqrt(rowSums(X2*(X2%*%v)))
f2L <- f2 - se2*1.96
f2U <- f2 + se2*1.96

pdf("HANSEN20-8b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x2p,f2,lty=1,xaxs="i",yaxs="i",type="l",xlab="Percentage Disadvantaged",ylab="Reading Test Score",ylim=c(62,82),lwd=wd,bty="n",yaxt="n")
polygon(c(x2p,rev(x2p)),c(f2U,rev(f2L)),col=grey(.8),border=NA)
axis(side=1,lwd=wd)
axis(side=2,seq(60,85,5),lwd=wd)
lines(x2p,f2,lwd=wd)
dev.off()

postscript("HANSEN20-8b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x2p,f2,lty=1,xaxs="i",yaxs="i",type="l",xlab="Percentage Disadvantaged",ylab="Reading Test Score",ylim=c(62,82),lwd=wd,bty="n",yaxt="n")
polygon(c(x2p,rev(x2p)),c(f2U,rev(f2L)),col=grey(.8),border=NA)
axis(side=1,lwd=wd)
axis(side=2,seq(60,85,5),lwd=wd)
lines(x2p,f2,lwd=wd)
dev.off()


