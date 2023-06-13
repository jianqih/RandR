#####################################
### This file generates Figure 20.1
### Polynomial Estimates of Experience Profile
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

xm <- 45;
p1 <- 2
p2 <- 4
p3 <- 8
p4 <- 12
x <- as.matrix(seq(0,xm,0.5))
xw <- matrix(1,nrow=length(x),ncol=p4+1)
xb <- matrix(1,nrow=length(x),ncol=p4+1)
zw <- matrix(1,nrow=nw,ncol=p4+1)
zb <- matrix(1,nrow=nb,ncol=p4+1)
for (j in 1:p4){
  xwj <- qw^j
  zwj <- zw[,1:j]
  gamw <- solve(t(zwj)%*%zwj,t(zwj)%*%xwj)
  egamw <- xwj - zwj%*%gamw
  sigw <- sqrt((t(egamw)%*%egamw)/nw)
  zw[,j+1] <- egamw%*%(1/sigw)
  xw[,j+1] <- (x^j - xw[,1:j]%*%gamw)%*%(1/sigw)

  xbj <- qb^j
  zbj = zb[,1:j]
  gamb <- solve(t(zbj)%*%zbj,t(zbj)%*%xbj)
  egamb <- xbj - zbj%*%gamb
  sigb <- sqrt((t(egamb)%*%egamb)/nb)
  zb[,j+1] <- egamb%*%(1/sigb)
  xb[,j+1] <- (x^j - xb[,1:j]%*%gamb)%*%(1/sigb)
}

zw1 = zw[,1:(1+p1)];
zw2 = zw[,1:(1+p2)];
zw3 = zw[,1:(1+p3)];
zw4 = zw[,1:(1+p4)];
zb1 = zb[,1:(1+p1)];
zb2 = zb[,1:(1+p2)];
zb3 = zb[,1:(1+p3)];
zb4 = zb[,1:(1+p4)];
betaw1 <- solve(t(zw1)%*%zw1,t(zw1)%*%yw)
betaw2 <- solve(t(zw2)%*%zw2,t(zw2)%*%yw)
betaw3 <- solve(t(zw3)%*%zw3,t(zw3)%*%yw)
betaw4 <- solve(t(zw4)%*%zw4,t(zw4)%*%yw)
betab1 <- solve(t(zb1)%*%zb1,t(zb1)%*%yb)
betab2 <- solve(t(zb2)%*%zb2,t(zb2)%*%yb)
betab3 <- solve(t(zb3)%*%zb3,t(zb3)%*%yb)
betab4 <- solve(t(zb4)%*%zb4,t(zb4)%*%yb)
fw1 <- xw[,1:(1+p1)]%*%betaw1
fw2 <- xw[,1:(1+p2)]%*%betaw2
fw3 <- xw[,1:(1+p3)]%*%betaw3
fw4 <- xw[,1:(1+p4)]%*%betaw4
fb1 <- xb[,1:(1+p1)]%*%betab1
fb2 <- xb[,1:(1+p2)]%*%betab2
fb3 <- xb[,1:(1+p3)]%*%betab3
fb4 <- xb[,1:(1+p4)]%*%betab4

# Figure 20.1a

wd <- 1.4

pdf("HANSEN20-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,fw1,lty=6,xlim=c(0,xm),ylim=c(2.4,3.2),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",lwd=wd,bty="n")
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,lwd=wd)
lines(x,fw2,lty=2,lwd=wd)
lines(x,fw3,lty=1,lwd=wd)
lines(x,fw4,lty=5,lwd=wd)
legend("bottomright",c("polynomial (2)","polynomial (4)","polynomial (8)","polynomial (12)"),lty=c(6,2,1,5),lwd=wd,bty="n")
dev.off()

postscript("HANSEN20-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,fw1,lty=6,xlim=c(0,xm),ylim=c(2.4,3.2),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",lwd=wd,bty="n")
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,lwd=wd)
lines(x,fw2,lty=2,lwd=wd)
lines(x,fw3,lty=1,lwd=wd)
lines(x,fw4,lty=5,lwd=wd)
legend("bottomright",c("polynomial (2)","polynomial (4)","polynomial (8)","polynomial (12)"),lty=c(6,2,1,5),lwd=wd,bty="n")
dev.off()

# Figure 20.1b

pdf("HANSEN20-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,fb1,lty=6,xlim=c(0,xm),ylim=c(2.4,3.2),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",lwd=wd,bty="n")
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,lwd=wd)
lines(x,fb2,lty=2,lwd=wd)
lines(x,fb3,lty=1,lwd=wd)
lines(x,fb4,lty=5,lwd=wd)
legend("bottomright",c("polynomial (2)","polynomial (4)","polynomial (8)","polynomial (12)"),lty=c(6,2,1,5),lwd=wd,bty="n")
dev.off()

postscript("HANSEN20-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,fb1,lty=6,xlim=c(0,xm),ylim=c(2.4,3.2),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",lwd=wd,bty="n")
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,lwd=wd)
lines(x,fb2,lty=2,lwd=wd)
lines(x,fb3,lty=1,lwd=wd)
lines(x,fb4,lty=5,lwd=wd)
legend("bottomright",c("polynomial (2)","polynomial (4)","polynomial (8)","polynomial (12)"),lty=c(6,2,1,5),lwd=wd,bty="n")
dev.off()
