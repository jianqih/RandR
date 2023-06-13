#########################################################################
##  This file generates prep work for Figure 16.3ab, 16.4, 16.6a
##  Dickey-Fuller Distributions & Related
#########################################################################

n <- 10000
rep <- 1000000
np <- 1000
set.seed(682020)

tr <- as.matrix(1:(n-1))
X <- cbind(matrix(1,n-1,1),tr)
XX <- solve(crossprod(X))%*%t(X)

C1 <- matrix(0,rep,1)
C2 <- matrix(0,rep,1)
C3 <- matrix(0,rep,1)
T1 <- matrix(0,rep,1)
T2 <- matrix(0,rep,1)
T3 <- matrix(0,rep,1)
K1 <- matrix(0,rep,1)
K2 <- matrix(0,rep,1)
SP <- matrix(0,rep,1)

for (i in 1:rep){
  e <- rnorm(n)
  y <- cumsum(e)
  e <- e[2:n]
  y1 <- y[1:n-1];
  y2 <- y1 - mean(y1)
  y3 <- y1 - X%*%(XX%*%y1)
  n1 <- sum(y1*e)
  n2 <- sum(y2*e)
  n3 <- sum(y3*e)
  s1 <- sum(y1^2)
  s2 <- sum(y2^2)
  s3 <- sum(y3^2)
  T1[i] <- n1/sqrt(s1)
  T2[i] <- n2/sqrt(s2)
  T3[i] <- n3/sqrt(s3)
  C1[i] <- n*n1/s1
  C2[i] <- n*n2/s2
  C3[i] <- n*n3/s3
  e1 <- e - mean(e)
  e2 <- e - X%*%(XX%*%e)
  y1 <- cumsum(e1)
  y2 <- cumsum(e2)
  s1 <- mean(y1^2)/n
  s2 <- mean(y2^2)/n
  K1[i] <- s1
  K2[i] <- s2
  SP[i] <- cor(y,cumsum(rnorm(n)))
}

EKernel <- function(x,h) {
  u <- (x/h)^2
  k <- (1-u/5)*(u < 5)*(3/4/sqrt(5))
  f <- mean(k)/h
  return(f)
}
 
# Coefficient Densities
h1 <- 2*min(sd(C1),IQR(C1)/1.34)/(rep^.2)
h2 <- 2*min(sd(C2),IQR(C2)/1.34)/(rep^.2)
h3 <- 2*min(sd(C3),IQR(C3)/1.34)/(rep^.2)
f1 <- matrix(0,np,1)
f2 <- matrix(0,np,1)
f3 <- matrix(0,np,1)
x <- seq(-30,5,length=np)
for (j in 1:np)   {
  xj <- x[j]
  f1[j] <- EKernel(C1-xj,h1)
  f2[j] <- EKernel(C2-xj,h2)
  f3[j] <- EKernel(C3-xj,h3)
}

Dat3a <- matrix(0,np,4)
Dat3a[,1] <- x
Dat3a[,2] <- f1
Dat3a[,3] <- f2
Dat3a[,4] <- f3
write.table(Dat3a,file="figure16_3a.txt",sep="\t",row.names=FALSE,col.names=FALSE)

# T Densities
h1 <- 2*min(sd(T1),IQR(T1)/1.34)/(rep^.2)
h2 <- 2*min(sd(T2),IQR(T2)/1.34)/(rep^.2)
h3 <- 2*min(sd(T3),IQR(T3)/1.34)/(rep^.2)
f1 <- matrix(0,np,1)
f2 <- matrix(0,np,1)
f3 <- matrix(0,np,1)
x <- seq(-5,3,length=np)
for (j in 1:np)   {
  xj <- x[j]
  f1[j] <- EKernel(T1-xj,h1)
  f2[j] <- EKernel(T2-xj,h2)
  f3[j] <- EKernel(T3-xj,h3)
}

Dat3b <- matrix(0,np,4)
Dat3b[,1] <- x
Dat3b[,2] <- f1
Dat3b[,3] <- f2
Dat3b[,4] <- f3
write.table(Dat3b,file="figure16_3b.txt",sep="\t",row.names=FALSE,col.names=FALSE)

# KPSS Densities
h1 <- 0.9*min(sd(K1),IQR(K1)/1.34)/(rep^.2)
h2 <- 0.9*min(sd(K2),IQR(K2)/1.34)/(rep^.2)
f1 <- matrix(0,np,1)
f2 <- matrix(0,np,1)
x <- seq(0,.5,length=np)
for (j in 1:np)   {
  xj <- x[j]
  f1[j] <- EKernel(K1-xj,h1)
  f2[j] <- EKernel(K2-xj,h2)
}

Dat4 <- matrix(0,np,3)
Dat4[,1] <- x
Dat4[,2] <- f1
Dat4[,3] <- f2
write.table(Dat4,file="figure16_4.txt",sep="\t",row.names=FALSE,col.names=FALSE)

# Spurious Regression Density
h1 <- 2*min(sd(SP),IQR(SP)/1.34)/(rep^.2)
f1 <- matrix(0,np,1)
x <- seq(-1,1,length=np)
for (j in 1:np)   {
  f1[j] <- EKernel(SP-x[j],h1)
}

Dat6a <- matrix(0,np,2)
Dat6a[,1] <- x
Dat6a[,2] <- f1
write.table(Dat6a,file="figure16_6a.txt",sep="\t",row.names=FALSE,col.names=FALSE)

# T critical values
pv <- c(.0001,.001,.01,.02,.03,.04,.05,.07,.1,.15,.2,.3,.5,.7,.9,.99)
c1 <- as.matrix(quantile(T1,pv))
c2 <- as.matrix(quantile(T2,pv))
c3 <- as.matrix(quantile(T3,pv))
c <- cbind(c1,c2,c3)


sink("df.log")
cat("\n Dickey-Fuller tCritical Values\n")
cat("\n Critical Values\n")
cat("\n")
print(round(c,digits=2))
cat("\n")
sink()

# KPSS critical values

pv1 <- 1-pv
c1 <- as.matrix(quantile(K1,pv1))
c2 <- as.matrix(quantile(K2,pv1))
c <- cbind(c1,c2)

sink("kpss.log")
cat("\n KPSS Critical Values\n")
cat("\n")
print(round(c,digits=3))
cat("\n")
sink()