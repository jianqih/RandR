#################################################
### This file generates Figure 7.4b
### Wage on Experience Regression Intervals
#################################################
### Uses data file cps09mar.txt
#########################################

dat <- as.matrix(read.delim("cps09mar.txt",header=FALSE))
experience <- dat[,1]-dat[,4]-6
mbf <- (dat[,11]==2)&(dat[,12]<=2)&(dat[,2]==1)
dat1 <- dat[mbf,]

#	Regression (3.12)
exp <- dat1[,1]-dat1[,4]-6
exp2 <- exp^2/100
y <- as.matrix(log(dat1[,5]/(dat1[,6]*dat1[,7])))
x <- cbind(dat1[,4],exp,exp2,matrix(1,nrow(dat1),1))
beta <- solve(t(x)%*%x,t(x)%*%y)

# Covariance Matrix (4.37)
e <- y-x%*%beta
leverage <- rowSums(x*(x%*%solve(t(x)%*%x)))
k <- ncol(x)
u3 <- x*((e/sqrt(1-leverage))%*%matrix(1,1,k))
xx <- solve(t(x)%*%x)
v3 <- xx %*% (t(u3)%*%u3) %*% xx

# Create Figure 
exp_seq <- seq(0,60)
xfit <- cbind(12,exp_seq,exp_seq^2/100,rep(1,61))
wage_fit <- xfit%*%beta
s <- apply(xfit,1,function(x) sqrt(t(x)%*%v3%*%x))
ub <- wage_fit + 1.96*s
lb <- wage_fit - 1.96*s

wd <- 1.4

pdf("HANSEN7-4b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(exp_seq,wage_fit,type='l',title=NULL,xaxs="i",yaxs="i",xaxt="n",
     xlab="Experience (Years)",ylab="Log Wage",xlim=c(0,60),ylim=c(2.2,2.8),bty="n",cex.lab=1.0,lwd=wd)
polygon(c(exp_seq,rev(exp_seq)),c(lb,rev(ub)),border=NA,col=gray(.8),lwd=wd)
axis(1,at=seq(0,60,by=10),lwd=wd)
axis(2,lwd=wd)
lines(exp_seq,wage_fit,lty=1,lwd=wd)
dev.off()

postscript("HANSEN7-4b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(exp_seq,wage_fit,type='l',title=NULL,xaxs="i",yaxs="i",xaxt="n",
     xlab="Experience (Years)",ylab="Log Wage",xlim=c(0,60),ylim=c(2.2,2.8),bty="n",cex.lab=1.0,lwd=wd)
polygon(c(exp_seq,rev(exp_seq)),c(lb,rev(ub)),border=NA,col=gray(.8),lwd=wd)
axis(1,at=seq(0,60,by=10),lwd=wd)
axis(2,lwd=wd)
lines(exp_seq,wage_fit,lty=1,lwd=wd)
dev.off()

