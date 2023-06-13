######################################################################
### This file generates Figure 2.4b
### Conditional density of log wage given experience (5,10,25,40)
### for White men with 12-years education
######################################################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
wm <- (dat[,11]==1)&(dat[,2]==0)&(dat[,4]==12)
dat1 <- dat[wm,]
y <- as.matrix(log(dat1[,5]/(dat1[,6]*dat1[,7])))
x <- as.matrix(dat1[,1]-dat1[,4]-6)
n <- length(y)

#######################################################
## Conditional Density estimated by Gaussian Kernel 
#######################################################

## bandwidth 
hx <- sd(x)/(n^(1/6))
hy <- sd(y)/(n^(1/6))

## evaluation points
yg <- seq(0.8,4.8,0.02)

## conditional densities
fx5 <- dnorm(x-5,sd=hx)
fx10 <- dnorm(x-10,sd=hx)
fx25 <- dnorm(x-25,sd=hx)
fy5 <- matrix(nrow=length(yg),ncol=1)
fy10 <- matrix(nrow=length(yg),ncol=1)
fy25 <- matrix(nrow=length(yg),ncol=1)
for (i in 1:length(yg)){
  fy <- dnorm(y-yg[i],sd=hy)
  fy5[i] <- mean(fy*fx5)/mean(fx5)
  fy10[i] <- mean(fy*fx10)/mean(fx10)
  fy25[i] <- mean(fy*fx25)/mean(fx25)
}

wd <- 1.4

leg1 <- expression(X==5)
leg2 <- expression(X==10)
leg3 <- expression(X==25)

pdf("HANSEN2-4b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(yg,fy5,lty=1,type="l",title=NULL,xaxs="i",yaxs="i",xlab="Log Dollars per Hour",
     ylab="",xlim=c(0.8,4.7),ylim=c(0,1.06),yaxt="n",xaxt="n",bty="n",lwd=wd)
lines(yg,fy10,lwd=wd)
lines(yg,fy25,lwd=wd)
axis(side=1,seq(1,4.5,0.5),lwd=wd)
text(1.3,.3,leg1)
text(1.8,.7,leg2)
text(3.8,.6,leg3)
arrows(1.5,.27,1.78,0.23,angle=20,length=.1,lwd=wd)
arrows(2.08,.68,2.37,0.64,angle=20,length=.1,lwd=wd)
arrows(3.6,.57,3.4,0.52,angle=20,length=.1,lwd=wd)
dev.off()

postscript("HANSEN2-4b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(yg,fy5,lty=1,type="l",title=NULL,xaxs="i",yaxs="i",xlab="Log Dollars per Hour",
     ylab="",xlim=c(0.8,4.7),ylim=c(0,1.06),yaxt="n",xaxt="n",bty="n",lwd=wd)
lines(yg,fy10,lwd=wd)
lines(yg,fy25,lwd=wd)
axis(side=1,seq(1,4.5,0.5),lwd=wd)
text(1.3,.3,leg1)
text(1.8,.7,leg2)
text(3.8,.6,leg3)
arrows(1.5,.27,1.78,0.23,angle=20,length=.1,lwd=wd)
arrows(2.08,.68,2.37,0.64,angle=20,length=.1,lwd=wd)
arrows(3.6,.57,3.4,0.52,angle=20,length=.1,lwd=wd)
dev.off()
