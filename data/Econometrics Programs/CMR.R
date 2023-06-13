#####################################
### This file generates empirical work related to threshold analysis of CMR (2008)
#####################################
### This file uses the package haven
### This file uses the data file CMR2008.dta
#####################################

library(haven)

nboot <- 1000
msize <- 500
trim <- .1
ngrid <- 100

dat <- read_dta("CMR2008.dta")
dat <- subset(dat, dat$samp_70==1)
v <- c("msa","city","chg_white_7080","fr_white_70","unem_70","pubtran_70","faminc_70","vac_70","rent_70","oneunit_70")
dat <- dat[v]
dat <- na.omit(dat)

msa <- dat$msa
MSA <- unique(msa)
N <- length(MSA)
s <- vector()
for (i in 1:N) {
 mi <- MSA[i]
 dati <- subset(dat, msa==mi)
 if (nrow(dati) > msize) s=c(s,mi)
}
MSA <- s
N <- length(MSA)

for (i in 1:N) {
 mi <- MSA[i]
 dati <- subset(dat, msa==mi)
 ni <- nrow(dati)

 city <- dati$city
 city <- city[1]
 changeW <- dati$chg_white_7080
 white <- dati$fr_white_70
 white2 <- white^2
 unem <- dati$unem_70
 pubtran <- dati$pubtran_70
 income <- dati$faminc_70
 vacancy <- dati$vac_70
 rent <- dati$rent_70
 oneunit <- dati$oneunit_70

 reg0 <- lm(changeW~white+white2+unem+pubtran+income+vacancy+rent+oneunit)
 e0 <- reg0$residuals
 sig0 <- mean(e0^2)
 n <- length(e0)
 q <- seq(quantile(white,trim),quantile(white,1-trim),length=ngrid)
 F <- vector(,ngrid)
 for (j in 1:ngrid) {
   d <- (white > q[j])
   Dwhite <- white*d
   Dwhite2 <- white2*d
   reg1 <- lm(changeW~white+white2+Dwhite+Dwhite2+unem+pubtran+income+vacancy+rent+oneunit)
   e1 <- reg1$residuals
   sig1 <- mean(e1^2)
   F[j] <- n*(sig0-sig1)/sig1
 }
 g <- q[which.max(F)]
 F <- max(F)
 d <- (white > g)
 Dwhite <- white*d
 Dwhite2 <- white2*d
 reg1 <- lm(changeW~white+white2+Dwhite+Dwhite2+unem+pubtran+income+vacancy+rent+oneunit)
 e1 <- reg1$residuals
 Fboot <- vector(,nboot)
 for (b in 1:nboot) {
   yb <- e1*rnorm(n)
   reg0 <- lm(yb~white+white2+unem+pubtran+income+vacancy+rent+oneunit)
   e0 <- reg0$residuals
   sig0 <- mean(e0^2)
   Fb <- vector(,ngrid)
   for (j in 1:ngrid) {
     d <- (white > q[j])
     Dwhite <- white*d
     Dwhite2 <- white2*d
     reg1 <- lm(yb~white+white2+Dwhite+Dwhite2+unem+pubtran+income+vacancy+rent+oneunit)
     e1 <- reg1$residuals
     sig1 <- mean(e1^2)
     Fb[j] <- n*(sig0-sig1)/sig1
   }
   Fboot[b] <- max(Fb)
 }
 bootp <- mean(Fboot > F)*100
 cat(ni,g,bootp,city,"\n")
}
