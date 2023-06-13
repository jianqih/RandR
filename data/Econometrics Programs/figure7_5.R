#################################################
### This file generates Figure 7.5
### Confidence Region for Return to Experience
### and Return to Education
#################################################


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

r <- matrix(c(100,0,0,100,0,20,0,0),nrow=2)
theta <- r%*%beta
vtheta <- r%*%v3%*%t(r)
vi <- solve(vtheta)
V11 <- vi[1,1]
V21 <- vi[2,1]
V22 <- vi[2,2]
c <- qchisq(.9,2)
xmax <- sqrt(c/(V11-(V21^2)/V22))

x1 <- matrix(seq(-1000,1000))*(xmax/1000)
x <- x1 + theta[1]

D <- sqrt(((x1^2)*((V21^2)/V22-V11) + c)/V22)
yL <- -x1*(V21/V22) - D + theta[2]
yU <- -x1*(V21/V22) + D + theta[2]

pdf("HANSEN7-5.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(theta[1],theta[2],title=NULL,xaxs="i",yaxs="i",xlab="Return to Education (%)",ylab="Return to Experience (%)",xlim=c(9.5,14),ylim=c(0,2.5),pch=19,col="black",bty="n",xaxt="n",cex.lab=.75,cex.axis=.75)
axis(side=1,seq(9,14,1),cex.axis=.75)
polygon(c(x,rev(x)),c(yL,rev(yU)),border=NA,col=gray(.8))
points(theta[1],theta[2],pch=19,col="black",cex=.8)
text(12,1.2,labels=expression(hat(beta)),cex=.8)
dev.off()

postscript("HANSEN7-5.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(theta[1],theta[2],title=NULL,xaxs="i",yaxs="i",xlab="Return to Education (%)",ylab="Return to Experience (%)",xlim=c(9.5,14),ylim=c(0,2.5),pch=19,col="black",bty="n",xaxt="n",cex.lab=.75,cex.axis=.75)
axis(side=1,seq(9,14,1),cex.axis=.75)
polygon(c(x,rev(x)),c(yL,rev(yU)),border=NA,col=gray(.8))
points(theta[1],theta[2],pch=19,col="black",cex=.8)
text(12,1.2,labels=expression(hat(beta)),cex=.8)
dev.off()



