#####################################
### This file generates Figure 20.6
### CV Function for Polynomial Estimates of Experience Profile
### College-Educated Women
#####################################
### Uses data file cps09mar.txt
#####################################

dat <- read.table("cps09mar.txt")
wcollegef <- (dat[,2]==1)&(dat[,4]==16)&(dat[,11]==1)
bcollegef <- (dat[,2]==1)&(dat[,4]==16)&(dat[,11]==2)
datw <- dat[wcollegef,]
datb <- dat[bcollegef,]

yw <- as.matrix(log(datw[,5]/(datw[,6]*datw[,7])))
qw <- (datw[,1]-datw[,4]-6)
yb <- as.matrix(log(datb[,5]/(datb[,6]*datb[,7])))
qb <- (datb[,1]-datb[,4]-6)
nw <- length(yw)
nb <- length(yb)

pmax <- 8
zw <- matrix(1,nrow=nw,ncol=pmax+1)
zb <- matrix(1,nrow=nb,ncol=pmax+1)
cvw <- matrix(1,nrow=pmax,ncol=1)
cvb <- matrix(1,nrow=pmax,ncol=1)
for (j in 1:pmax){
  xwj <- qw^j
  zwj <- zw[,1:j]
  gamw <- solve(t(zwj)%*%zwj,t(zwj)%*%xwj)
  egamw <- xwj - zwj%*%gamw
  sigw <- sqrt((t(egamw)%*%egamw)/nw)
  zw[,j+1] <- egamw%*%(1/sigw)
  zj <- zw[,1:(j+1)]
  xx <- solve(t(zj)%*%zj)
  bj <- xx%*%(t(zj)%*%yw)
  ej <- (yw - zj%*%bj)/(1-rowSums(zj*(zj%*%xx)))
  cvw[j] <- sum(ej^2)

  xbj <- qb^j
  zbj = zb[,1:j]
  gamb <- solve(t(zbj)%*%zbj,t(zbj)%*%xbj)
  egamb <- xbj - zbj%*%gamb
  sigb <- sqrt((t(egamb)%*%egamb)/nb)
  zb[,j+1] <- egamb%*%(1/sigb)
  zj <- zb[,1:(j+1)]
  xx <- solve(t(zj)%*%zj)
  bj <- xx%*%(t(zj)%*%yb)
  ej <- (yb - zj%*%bj)/(1-rowSums(zj*(zj%*%xx)))
  cvb[j] <- sum(ej^2)
 
}

K <- (1:pmax)
cvwmin <- min(cvw)
cvwK <- which.min(cvw)
cvbmin <- min(cvb)
cvbK <- which.min(cvb)

wd <- 1.4

pdf("HANSEN20-6a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(K,cvw,lty=1,xaxs="i",yaxs="i",type="l",ylab="Cross-Validation Criterion",xlab="Polynomial Order",lwd=wd,bty="n",ylim=c(1220,1260),yaxt="n")
axis(side=1,lwd=wd)
axis(side=2,seq(1220,1260,10),lwd=wd)
points(cvwK,cvwmin,pch=19)
text(6,1230,"CV(K)")
dev.off()

postscript("HANSEN20-6a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(K,cvw,lty=1,xaxs="i",yaxs="i",type="l",ylab="Cross-Validation Criterion",xlab="Polynomial Order",lwd=wd,bty="n",ylim=c(1220,1260),yaxt="n")
axis(side=1,lwd=wd)
axis(side=2,seq(1220,1260,10),lwd=wd)
points(cvwK,cvwmin,pch=19)
text(6,1230,"CV(K)")
dev.off()


pdf("HANSEN20-6b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(K,cvb,lty=1,xaxs="i",yaxs="i",type="l",ylab="Cross-Validation Criterion",xlab="Polynomial Order",lwd=wd,bty="n",ylim=c(124,130),yaxt="n")
axis(side=1,lwd=wd)
axis(side=2,seq(124,130,2),lwd=wd)
points(cvbK,cvbmin,pch=19)
text(5,125.5,"CV(K)")
dev.off()

postscript("HANSEN20-6b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(K,cvb,lty=1,xaxs="i",yaxs="i",type="l",ylab="Cross-Validation Criterion",xlab="Polynomial Order",lwd=wd,bty="n",ylim=c(124,130),yaxt="n")
axis(side=1,lwd=wd)
axis(side=2,seq(124,130,2),lwd=wd)
points(cvbK,cvbmin,pch=19)
text(5,125.5,"CV(K)")
dev.off()
