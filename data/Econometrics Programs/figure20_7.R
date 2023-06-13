#####################################
### This file generates Figure 20.7
### Confidence Bands for Experience Profile
### College-Educated Women
#############################
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
pw <- 5
pb <- 3
x <- as.matrix(seq(0,xm,0.5))
G <- length(x)
xw <- matrix(1,nrow=G,ncol=pw+1)
xb <- matrix(1,nrow=G,ncol=pw+1)
zw <- matrix(1,nrow=nw,ncol=pw+1)
zb <- matrix(1,nrow=nb,ncol=pw+1)
for (j in 1:pw){
  xwj <- qw^j
  zwj <- zw[,1:j]
  gamw <- solve(t(zwj)%*%zwj,t(zwj)%*%xwj)
  egamw <- xwj - zwj%*%gamw
  sigw <- sqrt((t(egamw)%*%egamw)/nw)
  zw[,j+1] <- egamw%*%(1/sigw)
  xw[,j+1] <- (x^j - xw[,1:j]%*%gamw)%*%(1/sigw)

  xbj <- qb^j
  zbj <- zb[,1:j]
  gamb <- solve(t(zbj)%*%zbj,t(zbj)%*%xbj)
  egamb <- xbj - zbj%*%gamb
  sigb <- sqrt((t(egamb)%*%egamb)/nb)
  zb[,j+1] <- egamb%*%(1/sigb)
  xb[,j+1] <- (x^j - xb[,1:j]%*%gamb)%*%(1/sigb)
}

xxw <- solve(t(zw)%*%zw)
betaw <- xxw%*%(t(zw)%*%yw)
fw <- xw%*%betaw
ew <- yw - zw%*%betaw
epw <- ew/(1-rowSums(zw*(zw%*%xxw)))
uw <- zw*(epw%*%matrix(1,1,ncol(zw)))
vw <- xxw%*%(t(uw)%*%uw)%*%xxw

zb = zb[,1:(1+pb)];
xb = xb[,1:(1+pb)];
xxb <- solve(t(zb)%*%zb)
betab <- xxb%*%(t(zb)%*%yb)
fb <- xb%*%betab
eb <- yb - zb%*%betab
epb <- eb/(1-rowSums(zb*(zb%*%xxb)))
ub <- zb*(epb%*%matrix(1,1,ncol(zb)))
vb <- xxb%*%(t(ub)%*%ub)%*%xxb

sew <- sqrt(rowSums(xw*(xw%*%vw)))
seb <- sqrt(rowSums(xb*(xb%*%vb)))

Lw <- fw - sew*1.96
Uw <- fw + sew*1.96
Lb <- fb - seb*1.96
Ub <- fb + seb*1.96

wd <- 1.4

pdf("HANSEN20-7a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,fw,lty=1,xlim=c(0,xm),ylim=c(2.4,3.3),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",lwd=wd,bty="n",yaxt="n")
polygon(c(x,rev(x)),c(Uw,rev(Lw)),col=grey(.8),border=NA)
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,seq(2.4,3.4,.2),lwd=wd)
lines(x,fw,lwd=wd)
dev.off()

postscript("HANSEN20-7a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,fw,lty=1,xlim=c(0,xm),ylim=c(2.4,3.3),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",lwd=wd,bty="n",yaxt="n")
polygon(c(x,rev(x)),c(Uw,rev(Lw)),col=grey(.8),border=NA)
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,seq(2.4,3.4,.2),lwd=wd)
lines(x,fw,lwd=wd)
dev.off()

pdf("HANSEN20-7b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,fb,lty=1,xlim=c(0,xm),ylim=c(2.4,3.3),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",lwd=wd,bty="n",yaxt="n")
polygon(c(x,rev(x)),c(Ub,rev(Lb)),col=gray(.8),border=NA)
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,seq(2.4,3.4,.2),lwd=wd)
lines(x,fb,lwd=wd)
dev.off()

postscript("HANSEN20-7b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,fb,lty=1,xlim=c(0,xm),ylim=c(2.4,3.3),xlab="Experience (Years)",ylab="Log Dollars per Hour",xaxs="i",yaxs="i",type="l",lwd=wd,bty="n",yaxt="n")
polygon(c(x,rev(x)),c(Ub,rev(Lb)),col=gray(.8),border=NA)
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,seq(2.4,3.4,.2),lwd=wd)
lines(x,fb,lwd=wd)
dev.off()
