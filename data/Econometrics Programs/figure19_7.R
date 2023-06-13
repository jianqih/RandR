#########################################################################
##  This file generates Figure 19.7
##  Testscores as a function of Initial Percentile
#########################################################################
### Uses package haven
### Uses data file DDK2011.dta
#########################################

library(haven)

data <- read_dta("DDK2011.dta")
tgirls <- subset(data,tracking==1 & girl==1)
y <- as.matrix(tgirls$totalscore)
x <- as.matrix(tgirls$percentile)
schoolid <- as.matrix(tgirls$schoolid)

# Reference Rule
S <- 100
n <- nrow(y)
x1 <- matrix(1,n,1)
xt = x/100
zz <- cbind(x1,xt,xt^2,xt^3,xt^4)
beta <- solve((t(zz)%*%zz),(t(zz)%*%y))
b <- mean(((beta[3]+xt*3*beta[4]+(xt^2)*6*beta[5])^2)*xt)
e <- y - zz%*%beta
sig <- (sum(e^2))/(n-5)
hrot <- S*0.58*((sig/n/b)^.2)

cat("Sample size")
print(n)
cat("ROT h")
print(hrot)

# Cross-Validation (conventional & clustered)
g<- 201
h1 <- 4
h2 <- 20
hh <- seq(h1,h2,(h2-h1)/g)
hn <- length(hh)
si <- (1:n)
CV1 <- matrix(0,n,hn)
CV2 <- matrix(0,n,hn)
for (i in 1:hn){
hi <- hh[i]
for (j in 1:n){
  xj <- x-x[j]
  k1 <- dnorm(xj/hi)
  k1[j] <- 0
  z <- cbind(x1,xj)
  zk1 <- z*(k1%*%cbind(1,1))
  beta1 <- solve(t(zk1)%*%z,t(zk1)%*%y)
  CV1[j,i] <- (y[j]-beta1[1])^2
  k2 <- k1*(schoolid != schoolid[j])
  zk2 <- z*(k2%*%cbind(1,1))
  beta2 <- solve(t(zk2)%*%z,t(zk2)%*%y)
  CV2[j,i] <- (y[j]-beta2[1])^2
}
}
cv1 <- colMeans(CV1)
cv1 <- cv1 - min(cv1)
hm1 <- hh[which.min(cv1)]
cat("Conventional CV h")
print(hm1)
cv2 <- colMeans(CV2)
cv2 <- cv2 - min(cv2)
hm2 <- hh[which.min(cv2)]
cat("Clustered CV h")
print(hm2)

wd <- 1.4

pdf("HANSEN19-7a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(hh,cv1,type="l",lty=5,ylab="Cross-Validation Criterion",xlab="Bandwidth",xaxs="i",yaxs="i",xlim=c(4,20),ylim=c(0,0.1),xaxt="n",bty="n",lwd=wd)
lines(hh,cv2,lwd=wd)
legend("top",legend=c("CV(h)","Clustered CV(h)"),lty=c(5,1),bg="white",bty="n",lwd=wd)
axis(side=1,seq(4,20,2),lwd=wd)
axis(side=2,lwd=wd)
dev.off()

postscript("HANSEN19-7a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(hh,cv1,type="l",lty=5,ylab="Cross-Validation Criterion",xlab="Bandwidth",xaxs="i",yaxs="i",xlim=c(4,20),ylim=c(0,0.1),xaxt="n",bty="n",lwd=wd)
lines(hh,cv2,lwd=wd)
legend("top",legend=c("CV(h)","Clustered CV(h)"),lty=c(5,1),bg="white",bty="n",lwd=wd)
axis(side=1,seq(4,20,2),lwd=wd)
axis(side=2,lwd=wd)
dev.off()

# Regression Estimation
h1 <- hm1
h2 <- hm2
g <- 201
xg <- seq(0,100,100/(g-1))
m1 <- matrix(0,g,1)
m2 <- matrix(0,g,1)
for (j in 1:g){
  xj <- x-xg[j]
  z <- cbind(x1,xj)
  z1 <- z*(dnorm(xj/h1)%*%cbind(1,1))
  z2 <- z*(dnorm(xj/h2)%*%cbind(1,1))
  beta1 <- solve(t(z1)%*%z,t(z1)%*%y)
  beta2 <- solve(t(z2)%*%z,t(z2)%*%y)
  m1[j,1] <- beta1[1]
  m2[j,1] <- beta2[1]
}


h <- hm2
m <- m2

# Prediction Error Estimation
e <- matrix(0,n,1)
for (j in 1:n){
  xj <- x-x[j]
  k1 <- dnorm(xj/h)
  k2 <- k1*(schoolid != schoolid[j])
  z <- cbind(x1,xj)
  zk <- z*(k2%*%cbind(1,1))
  beta <- solve(t(zk)%*%z,t(zk)%*%y)
  e[j,1] <- y[j]-beta[1]
}

# Variance Estimation - Unclustered
se1 <- matrix(0,g,1)
for (j in 1:g){
  xj <- x-xg[j]
  z <- cbind(x1,xj)
  k <- dnorm(xj/h)
  z1 <- z*(k%*%cbind(1,1))
  ZKZ <- solve(t(z1)%*%z)
  ze <- z*((k*e)%*%cbind(1,1))
  ZK1 <- t(ze)%*%ze
  V1 <- ZKZ %*% ZK1 %*% ZKZ
  se1[j,1] = sqrt(V1[1,1])
}
L1 <- m-1.96*se1
U1 <- m+1.96*se1

# Variance Estimation - Clustered
se2 <- matrix(0,g,1)
for (j in 1:g){
  xj <- x-xg[j]
  z <- cbind(x1,xj)
  k <- dnorm(xj/h)
  z1 <- z*(k%*%cbind(1,1))
  ZKZ <- solve(t(z1)%*%z)
  ze <- z*((k*e)%*%cbind(1,1))
  Re <- rowsum(ze,schoolid)
  ZK2 <- t(Re)%*%Re
  V2 <- ZKZ %*% ZK2 %*% ZKZ
  se2[j,1] = sqrt(V2[1,1])
}

# Plot Confidence Bands
L2 <- m-1.96*se2
U2 <- m+1.96*se2

z <- cbind(x1,x)
beta <- solve((t(z)%*%z),(t(z)%*%y))
mL <- beta[1]+xg*beta[2]

pdf("HANSEN19-7b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xg,m,type="l",lty=1,ylab="Testscore",xlab="Initial Percentile",xaxs="i",yaxs="i",xlim=c(0,100),ylim=c(0,30),xaxt="n",bty="n",lwd=wd)
polygon(c(xg,rev(xg)),c(L2,rev(U2)),border=NA,col=gray(.8),lwd=wd)
lines(xg,m,lwd=wd)
lines(xg,mL,lty=5,lwd=wd)
legend("topleft",legend=c("Local Linear Estimate","Global Linear Estimate"),lty=c(1,5),bg="white",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,lwd=wd)
dev.off()

postscript("HANSEN19-7b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xg,m,type="l",lty=1,ylab="Testscore",xlab="Initial Percentile",xaxs="i",yaxs="i",xlim=c(0,100),ylim=c(0,30),xaxt="n",bty="n",lwd=wd)
polygon(c(xg,rev(xg)),c(L2,rev(U2)),border=NA,col=gray(.8),lwd=wd)
lines(xg,m,lwd=wd)
lines(xg,mL,lty=5,lwd=wd)
legend("topleft",legend=c("Local Linear Estimate","Global Linear Estimate"),lty=c(1,5),bg="white",bty="n",lwd=wd)
axis(side=1,seq(0,100,10),lwd=wd)
axis(side=2,lwd=wd)
dev.off()

