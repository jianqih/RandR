#########################################################################
##  This file generates Figure 20.2b
##  Linear Spline of Transfers on Income
#########################################################################
##  Uses package haven
##  Uses data file CHJ2004.dta
#########################################################################library(haven)

library(haven)

dat <- read_dta("CHJ2004.dta")
y <- as.matrix(dat$transfers)
i <- as.matrix(dat$income)
n <- length(y)
xx <- cbind(matrix(1,n,1),dat$primary,dat$somesecondary,dat$secondary,dat$someuniversity,dat$university,dat$age,dat$female,dat$married,dat$child1,dat$child7,dat$child15,dat$size,dat$bothwork,dat$notemployed,dat$marriedf)

knots <- c(10000,20000,50000,100000,150000)
kn <- length(knots)
q <- seq(1,200000,1)
qn <- length(q)
ii <- matrix(1,n,kn)
qq <- matrix(1,qn,kn)
for (j in 1:kn) {
  knot <- knots[j]
  ii[,j] <- (i-knot)*(i>knot)
  qq[,j] <- (q-knot)*(q>knot)
}

x <- cbind(i,ii,xx)
beta <- qr.solve(x,y)
mx <- matrix(1,qn,1)%*%t(as.matrix(colMeans(xx)))%*%beta[(2+kn):nrow(beta)]
tr <- cbind(q,qq)%*%beta[1:(1+kn)] + mx
tk <- tr[knots]

wd <- 1.4

pdf("HANSEN20-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(knots,tk,xlim=c(0,200000),ylim=c(4000,15000),ylab="Transfers (Pesos)",xlab="Total Income (Pesos)",xaxs="i",yaxs="i",pch=19,cex=.7,lwd=wd,bty="n",yaxt="n")
lines(q,tr,lwd=wd)
axis(side=1,lwd=wd)
axis(side=2,seq(4000,16000,4000),lwd=wd)
legend("topright",c("knots","linear spline"),lty=c(0,1),pch=c(19,NA),pt.cex=.7,lwd=wd,bty="n")
dev.off()

postscript("HANSEN20-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(knots,tk,xlim=c(0,200000),ylim=c(4000,15000),ylab="Transfers (Pesos)",xlab="Total Income (Pesos)",xaxs="i",yaxs="i",pch=19,cex=.7,lwd=wd,bty="n",yaxt="n")
lines(q,tr,lwd=wd)
axis(side=1,lwd=wd)
axis(side=2,seq(4000,16000,4000),lwd=wd)
legend("topright",c("knots","linear spline"),lty=c(0,1),pch=c(19,NA),pt.cex=.7,lwd=wd,bty="n")
dev.off()
