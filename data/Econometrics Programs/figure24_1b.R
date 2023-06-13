#####################################
### This file generates Figure 24.1b
### LAD Criterion
#####################################
### Uses data file cps09mar.txt
#####################################

dat <- read.table("cps09mar.txt")
experience <- dat[,1]-dat[,4]-6
mbf <- (dat[,11]==2)&(dat[,12]<=2)&(dat[,2]==1)&(experience==12)
y <- as.matrix(log(dat[,5]/(dat[,6]*dat[,7])))
x <- dat[,4]
s <- c(1,6,8,9,10,13,20)
ys <- y[mbf]
ys <- ys[s]
xs <- x[mbf]
xs <- xs[s]
z <- sort(ys/xs)

n <- length(z)
f <- matrix(0,n,1)
for (i in 1:n){
  f[i] <- mean(abs(ys - xs*z[i]))
}
beta <- z[which.min(f)]
fmin <- min(f)

wd <- 1.4

pdf("HANSEN24-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(z,f,type="b",lty=1,xaxs="i",yaxs="i",ylab="",xlab=expression(beta),yaxt="n",xaxt="n",bty="n",xlim=c(.165,.225),ylim=c(.18,.45),cex=.7,lwd=wd)
axis(side=1,seq(.16,.23,.01),lwd=wd)
text(.217,.3,expression(M[n](beta)))
arrows(beta,fmin-.006,beta,.18,code=2,length=0.1,lwd=wd)
dev.off()

postscript("HANSEN24-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(z,f,type="b",lty=1,xaxs="i",yaxs="i",ylab="",xlab=expression(beta),yaxt="n",xaxt="n",bty="n",xlim=c(.165,.225),ylim=c(.18,.45),cex=.7,lwd=wd)
axis(side=1,seq(.16,.23,.01),lwd=wd)
text(.217,.3,expression(M[n](beta)))
arrows(beta,fmin-.006,beta,.18,code=2,length=0.1,lwd=wd)
dev.off()
