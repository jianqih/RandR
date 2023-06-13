#################################################
### This file generates Figure 7.4a
### Wage on Education Regression Intervals
#################################################
### Uses data file cps09mar.txt
#########################################

dat <- as.matrix(read.delim("cps09mar.txt",header=FALSE))
experience <- dat[,1]-dat[,4]-6
mbf <- (dat[,11]==2)&(dat[,12]<=2)&(dat[,2]==1)&(experience==12)
dat1 <- dat[mbf,]

#	Regression (3.12)
y <- as.matrix(log(dat1[,5]/(dat1[,6]*dat1[,7])))
x <- cbind(dat1[,4],matrix(1,nrow(dat1),1))
beta <- solve(t(x)%*%x,t(x)%*%y)

# Covariance Matrix (4.37)
e <- y-x%*%beta
leverage <- rowSums(x*(x%*%solve(t(x)%*%x)))
k <- ncol(x)
u3 <- x*((e/sqrt(1-leverage))%*%matrix(1,1,k))
xx <- solve(t(x)%*%x)
v3 <- xx %*% (t(u3)%*%u3) %*% xx

# Create Figure 
edu_seq <- cbind(seq(6,20),rep(1,15))
wage_fit <- edu_seq%*%beta
s <- apply(edu_seq,1,function(x) sqrt(t(x)%*%v3%*%x))
ub <- wage_fit + 1.96*s
lb <- wage_fit - 1.96*s

xplot <- seq(6,20)

wd <- 1.4

pdf("HANSEN7-4a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xplot,wage_fit,type='l',title=NULL,xaxs="i",yaxs="i",xaxt="n",
     xlab="Education (Years)",ylab="Log Wage", xlim=c(6,20), ylim=c(1,4),bty="n",cex.lab=1.0,lwd=wd)
polygon(c(xplot,rev(xplot)),c(lb,rev(ub)),border=NA,col=gray(.8),lwd=wd)
lines(xplot,wage_fit,lty=1,lwd=wd)
axis(1,at=seq(6,20,by=2),lwd=wd)
axis(2,lwd=wd)
dev.off()

postscript("HANSEN7-4a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xplot,wage_fit,type='l',title=NULL,xaxs="i",yaxs="i",xaxt="n",
     xlab="Education (Years)",ylab="Log Wage", xlim=c(6,20), ylim=c(1,4),bty="n",cex.lab=1.0,lwd=wd)
polygon(c(xplot,rev(xplot)),c(lb,rev(ub)),border=NA,col=gray(.8),lwd=wd)
lines(xplot,wage_fit,lty=1,lwd=wd)
axis(1,at=seq(6,20,by=2),lwd=wd)
axis(2,lwd=wd)
dev.off()
