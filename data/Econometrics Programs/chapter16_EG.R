#########################################################################
##  This file calculates Table 16.5
##  Engle-Granger Cointegration Test Critical Values
#########################################################################

n <- 10000
rep <- 1000000
set.seed(812020)
pv <- c(.0001,.001,.01,.02,.03,.04,.05,.07,.1,.15,.2,.3,.5,.7,.9,.99)

np <- length(pv)
critc <- matrix(0,np,3)
critt <- matrix(0,np,3)

t1 <- matrix(1,n,1)
tt1 <- solve(crossprod(t1))%*%t(t1)
t2 <- cbind(t1,as.matrix(1:n))
tt2 <- solve(crossprod(t2))%*%t(t2)

for (m in 2:4) {
C <- matrix(0,rep,1)
T <- matrix(0,rep,1)

for (i in 1:rep){
  e <- matrix(rnorm(n*m),n,m)
  y <- apply(e,2,cumsum)
  ym <- y - t1%*%(tt1%*%y)
  yt <- y - t2%*%(tt2%*%y)
  y1m <- ym[,1]
  y2m <- ym[,2:m]
  y1t <- yt[,1]
  y2t <- yt[,2:m]
  zetam <- solve(crossprod(y2m))%*%crossprod(y2m,y1m)
  zetat <- solve(crossprod(y2t))%*%crossprod(y2t,y1t)
  Vm <- y1m - y2m%*%zetam
  Vt <- y1t - y2t%*%zetat
  C[i] <- sum(Vm[1:(n-1)]*(Vm[2:n]-Vm[1:(n-1)]))/sqrt(sum(Vm[1:(n-1)]^2))/sqrt(1+t(zetam)%*%zetam)
  T[i] <- sum(Vt[1:(n-1)]*(Vt[2:n]-Vt[1:(n-1)]))/sqrt(sum(Vt[1:(n-1)]^2))/sqrt(1+t(zetat)%*%zetat)
}

critc[,m-1] <- quantile(C,pv)
critt[,m-1] <- quantile(T,pv)
}

sink("eg.log")
cat("\n Engle-Granger t Critical Values\n")
cat("\n")
cat("Case with Fitted Intercept\n")
print(round(critc,digits=2))
cat("\n")
cat("Case with Intercept and Trend\n")
print(round(critt,digits=2))
cat("\n")
sink()
