######################################
### This file generates Figure 10.1 
### Bootstrap Distributions of 
### (a) beta_1_hat
### (b) mu_hat
######################################
### Uses data file bootreps.txt
#########################################

bootreps <- as.matrix(read.table("bootreps.txt", quote="\"", comment.char=""))
bootreps_vec <- matrix(t(bootreps),1)
b1<-bootreps_vec[1]
betas1<-bootreps_vec[2:10001]
mu<-bootreps_vec[10002]
theta4<-bootreps_vec[10003:20002]

bw1 <- bw.SJ(betas1)
f1 <- density(betas1,bw=bw1,n=1001,from=0,to=.3)
bw2 <- bw.SJ(theta4)
f2 <- density(theta4,bw=bw2,n=1001,from=18,to=35)

qL1 <- quantile(betas1,0.025)
qU1 <- quantile(betas1,0.975)
fL1 <- f1$y[sum(f1$x<qL1)]
fU1 <- f1$y[sum(f1$x<qU1)]
fB1 <- f1$y[which.min(f1$x<b1)]

qL2 <- quantile(theta4,0.025)
qU2 <- quantile(theta4,0.975)
fL2 <- f2$y[which.min(f2$x<qL2)]
fU2 <- f2$y[which.min(f2$x<qU2)]
fT <- f2$y[which.min(f2$x<mu)]

lwd <- 1.4

pdf("HANSEN10-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(f1$x,f1$y,type="l",xlim=c(0,0.3),ylim=c(0,14),xlab="Return to Education",ylab="",xaxs="i",yaxs="i",yaxt="n",lwd=wd,bty="n")
axis(1,lwd=wd)
arrows(qL1,fL1,qL1,0,length=0.1,lwd=wd)
arrows(qU1,fU1,qU1,0,length=0.1,lwd=wd)
segments(b1,fB1,b1,0,lty=2,lwd=wd)
dev.off()

postscript("HANSEN10-1a.eps",paper="special",horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(f1$x,f1$y,type="l",xlim=c(0,0.3),ylim=c(0,14),xlab="Return to Education",ylab="",xaxs="i",yaxs="i",yaxt="n",lwd=wd,bty="n")
axis(1,lwd=wd)
arrows(qL1,fL1,qL1,0,length=0.1,lwd=wd)
arrows(qU1,fU1,qU1,0,length=0.1,lwd=wd)
segments(b1,fB1,b1,0,lty=2,lwd=wd)
dev.off()

pdf("HANSEN10-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(f2$x,f2$y,type="l",xlim=c(15,35),ylim=c(0,0.18),xlab="Mean Wage of College Graduates",ylab="",xaxs="i",yaxt="n",yaxs="i",lwd=wd,bty="n")
axis(1,lwd=wd)
arrows(qL2,fL2,qL2,0,length=0.1,lwd=wd)
arrows(qU2,fU2,qU2,0,length=0.1,lwd=wd)
segments(mu,fT,mu,0,lty=2,lwd=wd)
dev.off()

postscript("HANSEN10-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(f2$x,f2$y,type="l",xlim=c(15,35),ylim=c(0,0.18),xlab="Mean Wage of College Graduates",ylab="",xaxs="i",yaxt="n",yaxs="i",lwd=wd,bty="n")
axis(1,lwd=wd)
arrows(qL2,fL2,qL2,0,length=0.1,lwd=wd)
arrows(qU2,fU2,qU2,0,length=0.1,lwd=wd)
segments(mu,fT,mu,0,lty=2,lwd=wd)
dev.off()
