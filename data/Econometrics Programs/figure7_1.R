
############################################
### This file generates Figure 7.1 
### The least-squares estimmator beta_1_hat
### as a function of sample size n
############################################
### Uses data file cps09mar.txt
#########################################

data1 <- read.delim("cps09mar.txt",header=FALSE)

wm <- subset(data1, V2==0 & V11==1)
wm$hrwage <- wm$V5/wm$V6/wm$V7
wm$edu <- wm$V4
wm$exp <- wm$V1 - wm$edu - 6
sample0 <- wm[,c(which(colnames(wm)=="hrwage"), which(colnames(wm)=="edu"), which(colnames(wm)=="exp"))]
sample1 <- cbind(sample0, sample0[,3]^2)
n <- nrow(sample1)

## random sampling without replacement
set.seed(71)
s <- (1:n)
dat <- sample1[sample(s,n),]

Y0 <- log(dat[,which(colnames(sample1)=="hrwage")])
X0 <- as.matrix(cbind(rep(1,n),dat[,-1]))

b <- matrix(NA,n,1)
for(i in 5:n){
  X <- X0[1:i,]	## only the first int[i] observations are used
  Y <- Y0[1:i]
  beta <- solve(t(X)%*%X)%*%(t(X)%*%Y)
  b[i] <- beta[2]
}

pdf("HANSEN7-1.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
oldpar <- par(mar=c(5.1,5.1,4.1,2.1))
plot(s, b, type="l",title=NULL,xaxs="i",yaxs="i",xlab="Number of Observations",ylab="Coefficient Estimate", xlim=c(40,n), ylim=c(0.11,0.128),bty="n",xaxt="n",yaxt="n",cex.lab=.75)
axis(side=1,seq(0,25000,5000),cex.axis=.75)
axis(side=2,seq(.11,.13,.005),cex.axis=.75)
text(22000,.112,expression(hat(beta)[n]),cex=.8)
par(oldpar)
dev.off()

postscript("HANSEN7-1.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
oldpar <- par(mar=c(5.1,5.1,4.1,2.1))
plot(s, b, type="l",title=NULL,xaxs="i",yaxs="i",xlab="Number of Observations",ylab="Coefficient Estimate", xlim=c(40,n), ylim=c(0.11,0.128),bty="n",xaxt="n",yaxt="n",cex.lab=.75)
axis(side=1,seq(0,25000,5000),cex.axis=.75)
axis(side=2,seq(.11,.13,.005),cex.axis=.75)
text(22000,.112,expression(hat(beta)[n]),cex=.8)
par(oldpar)
dev.off()