#########################################
### This file generates Figure 3.5
### Impact of an influential observation
### on the least-squares estimator 
#########################################
### Uses the file "figure3_5.txt"
#########################################

## the artificial data was generated once and saved in "figure3_5.txt"
infdata <- read.table(file="figure3_5.txt")
Y <- infdata[,1]
X <- infdata[,2]
n <- length(Y)
XX <- cbind(matrix(1,n,1),X)
Y1 <- c(Y,0)
X1 <- c(X,9)
XX1 <- cbind(matrix(1,n+1,1),X1)
beta <- solve(t(XX)%*%XX)%*%(t(XX)%*%Y)
beta1 <- solve(t(XX1)%*%XX1)%*%(t(XX1)%*%Y1)

L <- .5
U <- 11

pdf("HANSEN3-5.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(X1,Y1,ylab="",xlab="",cex.lab=.75,cex.axis=.75,cex=.8,bty="n",yaxt="n",xaxt="n",xlim=c(0,10),ylim=c(-2.5,11))
lines(c(L,U),c(-1.7,-1.7))
lines(c(L,L),c(-1.7,11))
lines(c(L,U),c(beta[1]+beta[2]*L,beta[1]+beta[2]*U))
lines(c(L,U),c(beta1[1]+beta1[2]*L,beta1[1]+beta1[2]*U))
points(9,0,pch=20,cex=.8)
text(5.7,8.7,"Leave-One-Out OLS",cex=.75)
text(8.7,5.5,"Full Sample OLS",cex=.75)
text(8,2,"Influential Observation",cex=.75)
arrows(7.7,8.5,8.5,8,code=2,length=0.1)
arrows(8.8,6,8.5,6.8,code=2,length=0.1)
arrows(8,1.6,8.86,.3,code=2,length=.1)
text(0,10,"Y",cex=.75)
text(10,-2.5,"X",cex=.75)
dev.off()

postscript("HANSEN3-5.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(X1,Y1,ylab="",xlab="",cex.lab=.75,cex.axis=.75,cex=.8,bty="n",yaxt="n",xaxt="n",xlim=c(0,10),ylim=c(-2.5,11))
lines(c(L,U),c(-1.7,-1.7))
lines(c(L,L),c(-1.7,11))
lines(c(L,U),c(beta[1]+beta[2]*L,beta[1]+beta[2]*U))
lines(c(L,U),c(beta1[1]+beta1[2]*L,beta1[1]+beta1[2]*U))
points(9,0,pch=20,cex=.8)
text(5.7,8.7,"Leave-One-Out OLS",cex=.75)
text(8.7,5.5,"Full Sample OLS",cex=.75)
text(8,2,"Influential Observation",cex=.75)
arrows(7.7,8.5,8.5,8,code=2,length=0.1)
arrows(8.8,6,8.5,6.8,code=2,length=0.1)
arrows(8,1.6,8.86,.3,code=2,length=.1)
text(0,10,"Y",cex=.75)
text(10,-2.5,"X",cex=.75)
dev.off()
