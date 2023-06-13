#####################################
### This file generates empirical work related to threshold analysis of CMR (2008)
### It calculates threshold/tipping point model
### Creates Figure 23.3
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

ngrid <- 100
trim <- .1
conf = .99	# For threshold confidence interval

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
Y <- dat$chg_white_7080
y <- Within(Y,msa,MSA)
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
sse <- vector(,ngrid)
for (j in 1:ngrid) {
  qj <- qq[j]
  d <- Within((q > qj),msa,MSA)
  dminority <- Within(q*(q > qj),msa,MSA)
  xj <- cbind(d,dminority,x)
  e <- y - xj%*%solve(crossprod(xj),crossprod(xj,y))
  sse[j] <- mean(e^2)
}
g <- qq[which.min(sse)]
smin <- min(sse)
c <- -2*log(1-sqrt(conf))
sc <- smin*(1+c/n)
ss <- (sse > sc)
g1 <- qq[which.min(ss)]
g2 <- qq[ngrid+1-which.min(rev(ss))]
d <- Within((q > g),msa,MSA)
dminority <- Within((q-g)*(q > g),msa,MSA)
xj <- cbind(d,dminority,x)
xx <- solve(crossprod(xj))
beta <- xx%*%crossprod(xj,y)
e <- y - xj%*%beta
u <- xj*as.vector(e)
S <- rowsum(u,msa)
V <- xx%*%crossprod(S)%*%xx
se <- sqrt(diag(V)) 
Xj <- cbind((q > g),(q-g)*(q > g),q,q2,dat$unem_70,dat$pubtran_70,dat$faminc_70,dat$vac_70,dat$rent_70,dat$oneunit_70)
alpha <- as.numeric(mean(Y) - colMeans(Xj)%*%beta)

cat("Sample size, Number of MSAs, Number of GridPoints, Trimming Percentage", "\n")
print(c(n,N,ngrid,trim))
cat("Threshold Estimate, Confidence Interval, level", "\n")
print(c(g,g1,g2,conf))
cat("Intercept")
print(alpha)
cat("Parameter Estimates & Standard Errors", "\n")
print(cbind(beta,se))

wd <- 1.4

pdf("HANSEN23-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(qq,sse,type="l",lty=1,xaxs="i",yaxs="i",ylab="Average Squared Error Function",xlab="Threshold Coefficient",xlim=c(qL,qU),bty="n",xaxt="n",yaxt="n",ylim=c(3766,3773),lwd=wd,bty="n")
axis(1,seq(0,.6,.1),lwd=wd)
axis(2,seq(3764,3774,2),lwd=wd)
text(.27,3769,expression(paste(S[n](gamma))))
points(g,smin,pch=16,cex=.8)
text(g+.02,smin,expression(hat(gamma)))
dev.off()

postscript("HANSEN23-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(qq,sse,type="l",lty=1,xaxs="i",yaxs="i",ylab="Average Squared Error Function",xlab="Threshold Coefficient",xlim=c(qL,qU),bty="n",xaxt="n",yaxt="n",ylim=c(3766,3773),lwd=wd,bty="n")
axis(1,seq(0,.6,.1),lwd=wd)
axis(2,seq(3764,3774,2),lwd=wd)
text(.27,3769,expression(paste(S[n](gamma))))
points(g,smin,pch=16,cex=.8)
text(g+.02,smin,expression(hat(gamma)))
dev.off()

n1 <- 20
n2 <- 40
x1 <- seq(qL,g,length=n1)
x2 <- seq(g,qU,length=n2)
f1 <- x1*beta[3] + (x1^2)*beta[4] + alpha
f2 <- beta[1] + (x2-g)*beta[2] + x2*beta[3] + (x2^2)*beta[4] + alpha
s1 <- vector(,n1)
for (j in 1:n1){
  x = c(x1[j],x1[j]^2)
  s1[j] <- sqrt(t(x)%*%V[3:4,3:4]%*%x)
}
s2 <- vector(,n2)
for (j in 1:n2){
  x = c(1,x2[j]-g,x2[j],x2[j]^2)
  s2[j] <- sqrt(t(x)%*%V[1:4,1:4]%*%x)
}

pdf("HANSEN23-3b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x1,f1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Change in White Population, 1970-1980",xlab="Fraction Minority in 1970",xlim=c(qL,qU),bty="n",ylim=c(-20,25),yaxt="n",xaxt="n",lwd=wd)
polygon(c(x1,rev(x1)),c(f1+1.96*s1,rev(f1-1.96*s1)),col=gray(.8),border=NA)
polygon(c(x2,rev(x2)),c(f2+1.96*s2,rev(f2-1.96*s2)),col=gray(.8),border=NA)
axis(1,seq(0,.6,.1),lwd=wd)
axis(2,seq(-40,40,10),lwd=wd)
lines(x1,f1,lwd=wd)
lines(x2,f2,lwd=wd)
abline(v=g,lwd=wd)
abline(h=0,lty=2,lwd=wd)
dev.off()

postscript("HANSEN23-3b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x1,f1,type="l",lty=1,xaxs="i",yaxs="i",ylab="Change in White Population, 1970-1980",xlab="Fraction Minority in 1970",xlim=c(qL,qU),bty="n",ylim=c(-20,25),yaxt="n",xaxt="n",lwd=wd)
polygon(c(x1,rev(x1)),c(f1+1.96*s1,rev(f1-1.96*s1)),col=gray(.8),border=NA)
polygon(c(x2,rev(x2)),c(f2+1.96*s2,rev(f2-1.96*s2)),col=gray(.8),border=NA)
axis(1,seq(0,.6,.1),lwd=wd)
axis(2,seq(-40,40,10),lwd=wd)
lines(x1,f1,lwd=wd)
lines(x2,f2,lwd=wd)
abline(v=g,lwd=wd)
abline(h=0,lty=2,lwd=wd)
dev.off()
