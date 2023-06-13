#####################################
### This file does pre-work for Figure 28.2
### Coverage probability of PMS CIs
#####################################
### Creates file figure28_2.txt
#####################################


rep <- 1000000
n <- 30
bmax <- 1.2
rhos <- c(.3,.5,.7,.8)
cselect <- 3.84
ccrit <- 3.84
set.seed(2122021)

bs <- seq(0,bmax,by=(bmax/50))
bn <- length(bs)
rn <- length(rhos)
cp <- matrix(1,bn,rn)
Z <- matrix(1,n,1)

for (ri in 1:rn){
  rho <- rhos[ri]
  for (bi in 1:bn){
    b2 <- bs[bi]
    sd2 <- sqrt(1-rho^2)
    sig2 <- 1 + (b2^2)*(1-rho^2)
    pr <- matrix(0,rep,1)
    for (i in 1:rep){
      X1 <- rnorm(n)
      X2 <- X1*rho + rnorm(n,mean=0,sd=sd2)
      Y <- X2*b2 + rnorm(n)
      S <- cbind(X1,Z)
      L <- cbind(X1,X2,Z)
      MS <- solve(crossprod(S))
      ML <- solve(crossprod(L))
      betaS <- MS%*%crossprod(S,Y)
      betaL <- ML%*%crossprod(L,Y)
      VS <- MS*sig2
      VL <- ML
      t1L <- (betaL[1]^2 )/VL[1,1]
      t1S <- (betaS[1]^2 )/VS[1,1]
      t2 <- (betaL[2]^2 )/VL[2,2]
      if (t2 > cselect) T <- t1L else T <- t1S
      pr[i] <- (T < ccrit)
    }
    cp[bi,ri] <- mean(pr)
  }
}

dat = matrix(0,bn,rn+1)
dat[,1] = bs
dat[,2:(rn+1)] = cp
write.table(dat,file="figure28_2.txt",sep="\t",row.names=FALSE,col.names=FALSE)