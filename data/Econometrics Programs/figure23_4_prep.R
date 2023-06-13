#####################################
### This file generates empirical work related to threshold analysis of CMR (2008)
### It calculates the test for a threshold effect
### Creates prep work for figure 23.4
#####################################
### This file uses the package haven
### This file uses the data file CMR2008.dta
#####################################

library(haven)

Within <- function(x,id,uid) {
 xw <- x
 for (i in 1:length(uid)) {
   ii <- which(id == uid[i])
   xi <- x[ii] 
   xw[ii] <- xi - mean(xi)
 }
 return(xw)
}

nboot <- 10000
trim <- .1
ngrid <- 100
set.seed(6142021)

dat <- read_dta("CMR2008.dta")
dat <- subset(dat, dat$samp_70==1)
v <- c("msa","city","chg_white_7080","fr_min_70","unem_70","pubtran_70","faminc_70","vac_70","rent_70","oneunit_70")
dat <- dat[v]
dat <- na.omit(dat)
msa <- dat$msa
MSA <- unique(msa)
N <- length(MSA)

q <- dat$fr_min_70
q2 <- q^2
y <- Within(dat$chg_white_7080,msa,MSA)
minority <- Within(q,msa,MSA)
minority2 <- Within(q2,msa,MSA)
unem <- Within(dat$unem_70,msa,MSA)
pubtran <- Within(dat$pubtran_70,msa,MSA)
income <- Within(dat$faminc_70,msa,MSA)
vacancy <- Within(dat$vac_70,msa,MSA)
rent <- Within(dat$rent_70,msa,MSA)
oneunit <- Within(dat$oneunit_70,msa,MSA)

n <- length(y)
qL <- quantile(q,trim)
qU <- quantile(q,1-trim)
qq <- seq(qL,qU,length=ngrid)
x <- cbind(minority,minority2,unem,pubtran,income,vacancy,rent,oneunit)

e0 <- y - x%*%solve(crossprod(x),crossprod(x,y))
sig0 <- mean(e0^2)

Fs <- vector(,ngrid)
for (j in 1:ngrid) {
  qj <- qq[j]
  d <- Within((q > qj),msa,MSA)
  dminority <- Within(q*(q > qj),msa,MSA)
  xj <- cbind(d,dminority,x)
  e <- y - xj%*%solve(crossprod(xj),crossprod(xj,y))
  Fs[j] <- n*(sig0/mean(e^2) - 1)
}
g <- qq[which.max(Fs)]
F <- max(Fs)
d <- Within((q > g),msa,MSA)
dminority <- Within(q*(q > g),msa,MSA)
xj <- cbind(d,dminority,x)
e <- y - xj%*%solve(crossprod(xj),crossprod(xj,y))

nn <- rowsum(matrix(1,n,1),msa)
cn <- cumsum(nn)
S <- matrix(0,n,N)
S[1:cn[1],1] <- matrix(1,nn[1],1)
for (j in 2:N)  S[(cn[j-1]+1):cn[j],j] <- matrix(1,nn[j],1)

 Fboot <- vector(,nboot)
 Cboot <- vector(,nboot)
 for (b in 1:nboot) {
   yb <- e*rnorm(n)
   sigb <- mean((yb - x%*%solve(crossprod(x),crossprod(x,yb)))^2)
   Fb <- vector(,ngrid)
   yc <- e*as.vector(S%*%rnorm(N))
   sigc <- mean((yc - x%*%solve(crossprod(x),crossprod(x,yc)))^2)
   Cb <- vector(,ngrid)
   for (j in 1:ngrid) {
     qj <- qq[j]
     d <- Within((q > qj),msa,MSA)
     dminority <- Within(q*(q > qj),msa,MSA)
     xj <- cbind(d,dminority,x)
     eb <- yb - xj%*%solve(crossprod(xj),crossprod(xj,yb))
     Fb[j] <- n*(sigb/mean(eb^2) - 1)
     ec <- yc - xj%*%solve(crossprod(xj),crossprod(xj,yc))
     Cb[j] <- n*(sigc/mean(ec^2) - 1)
   }
   Fboot[b] <- max(Fb)
   Cboot[b] <- max(Cb)
 }
pv <- mean(Fboot > F)*100
qv <- quantile(Fboot,.99)
pvc <- mean(Cboot > F)*100
qvc90 <- quantile(Cboot,.90)
qvc95 <- quantile(Cboot,.95)
qvc99 <- quantile(Cboot,.99)
cq <- qchisq(.99,2)
print(c(F,pv,qv,pvc,qvc90,qvc95,qvc99))


Dat <- matrix(0,ngrid,2)
Dat[,1] <- qq
Dat[,2] <- Fs
write.table(Dat,file="figure23_4.txt",sep="\t",row.names=FALSE,col.names=FALSE)
write.table(Cboot,file="figure23_4_1.txt",,sep="\t",row.names=FALSE,col.names=FALSE)


