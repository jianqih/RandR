#########################################################################
##  This file calculates Table 16.7
##  Johansen Cointegration Test Critical Values
#########################################################################
##  Uses package geigen
#########################################################################

library(geigen)

n <- 10000
rep <- 1000000
set.seed(832020)
pv <- c(.0001,.001,.01,.02,.03,.04,.05,.07,.1,.15,.2,.3,.5,.7,.9)
nm <- 12

t1 <- matrix(1,n-1,1)
tt1 <- t(t1)/(n-1)
t2 <- (1:(n-1))
t2 <- t2 - mean(t2)

store2 <- matrix(0,rep,nm)
store3 <- matrix(0,rep,nm)
store4 <- matrix(0,rep,nm)

for (i in 1:rep){
  e <- matrix(rnorm(n*nm),n,nm)
  y <- apply(e,2,cumsum)
  yb <- y[1:(n-1),]
  ym <- yb - t1%*%(tt1%*%yb)
  dy <- e[2:n,]
  F2 <- cbind(t1,yb)
  F3 <- cbind(t2,ym)
  Q2 <- crossprod(F2)
  Q3 <- crossprod(F3)
  C2 <- crossprod(F2,dy)
  C3 <- crossprod(F3,dy)

  for (m in 1:nm) {
    M2 <- t(C2[1:(m+1),1:m])%*%solve(Q2[1:(m+1),1:(m+1)])%*%C2[1:(m+1),1:m]/n
    M3 <- t(C3[1:m,1:m])%*%solve(Q3[1:m,1:m])%*%C3[1:m,1:m]/n
    M4 <- t(C3[1:(m+1),1:m])%*%solve(Q3[1:(m+1),1:(m+1)])%*%C3[1:(m+1),1:m]/n
    eig2 <- eigen(M2,symmetric=TRUE,only.values=TRUE)
    eig3 <- eigen(M3,symmetric=TRUE,only.values=TRUE)
    eig4 <- eigen(M4,symmetric=TRUE,only.values=TRUE)
    store2[i,m] <- -n*sum(log(1-eig2$values))
    store3[i,m] <- -n*sum(log(1-eig3$values))
    store4[i,m] <- -n*sum(log(1-eig4$values))
  }
}

criticals2 <- apply(store2, 2, quantile, probs = 1-pv)
criticals3 <- apply(store3, 2, quantile, probs = 1-pv)
criticals4 <- apply(store4, 2, quantile, probs = 1-pv)

criticals2[,1:2] <- round(criticals2[,1:2],2)
criticals2[,3:7] <- round(criticals2[,3:7],1)
criticals2[,8:12] <- round(criticals2[,8:12],0)

criticals3[,1:2] <- round(criticals3[,1:2],2)
criticals3[,3:7] <- round(criticals3[,3:7],1)
criticals3[,8:12] <- round(criticals3[,8:12],0)

criticals4[,1] <- round(criticals4[,1],2)
criticals4[,2:6] <- round(criticals4[,2:6],1)
criticals4[,7:12] <- round(criticals4[,7:12],0)

sink("Johansen.log")
cat("\n Johansen Trace Test Critical Values\n")
cat("Replications, Sample Size")
print(cbind(rep,n))
cat("\n")

cat("Case 2: Constrained intercept\n")
print(criticals2)
cat("\n")

cat("Case 3: Intercept\n")
print(criticals3)
cat("\n")

cat("Case 4: Trend\n")
print(criticals4)
cat("\n")

sink()
