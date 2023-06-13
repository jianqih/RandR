#####################################
### This file generates Figure 16.8
### VECM
#####################################
### Uses package haven
### Uses dataset FRED-QD.dta
#########################################

library(haven)
data <- read_dta("FRED-QD.dta")
x <- as.matrix(data$gs10)
y <- as.matrix(data$tb3ms)
n <- length(y)

X <- cbind(matrix(1,n,1),x)
beta <- solve(crossprod(X))%*%crossprod(X,y)
xx <- rbind(0,16)
beta1 <- 1.01
beta2 <- 1.58
p <- xx%*%beta1 - beta2
alpha1 <- -.095
alpha2 <- .069

c <- 2
x1 <- 10
y1 <- 4
z1 <- y1 - x1*beta1 + beta2
dy1 <- alpha1*z1
dx1 <- alpha2*z1
x2 <- 4
y2 <- 8
z2 <- y2 - x2*beta1 + beta2
dy2 <- alpha1*z2
dx2 <- alpha2*z2
x3 <- 14
y3 <- 7
z3 <- y3 - x3*beta1 + beta2
dy3 <- alpha1*z3
dx3 <- alpha2*z3
x4 <- 8
y4 <- 13
z4 <- y4 - x4*beta1 + beta2
dy4 <- alpha1*z4
dx4 <- alpha2*z4


pdf("HANSEN16-8.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,y,xlab="10-Year Interest Rate",ylab="3-Month Interest Rate",bty="n",xaxs="i",yaxs="i",yaxt="n",xaxt="n",xlim=c(0,16),ylim=c(0,16),cex.lab=.75,cex=.8)
axis(side=1,seq(0,16,2),cex.axis=.75)
axis(side=2,seq(0,16,2),cex.axis=.75)
lines(xx,p)
points(x1,y1,pch=19,col="black",cex=.75)
arrows(x1,y1,x1+dx1*c,y1+dy1*c,code=2,length=0.1)
points(x2,y2,pch=19,col="black",cex=.75)
arrows(x2,y2,x2+dx2*c,y2+dy2*c,code=2,length=0.1)
points(x3,y3,pch=19,col="black",cex=.75)
arrows(x3,y3,x3+dx3*c,y3+dy3*c,code=2,length=0.1)
points(x4,y4,pch=19,col="black",cex=.75)
arrows(x4,y4,x4+dx4*c,y4+dy4*c,code=2,length=0.1)
dev.off()

postscript("HANSEN16-8.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,y,xlab="10-Year Interest Rate",ylab="3-Month Interest Rate",bty="n",xaxs="i",yaxs="i",yaxt="n",xaxt="n",xlim=c(0,16),ylim=c(0,16),cex.lab=.75,cex=.8)
axis(side=1,seq(0,16,2),cex.axis=.75)
axis(side=2,seq(0,16,2),cex.axis=.75)
lines(xx,p)
points(x1,y1,pch=19,col="black",cex=.75)
arrows(x1,y1,x1+dx1*c,y1+dy1*c,code=2,length=0.1)
points(x2,y2,pch=19,col="black",cex=.75)
arrows(x2,y2,x2+dx2*c,y2+dy2*c,code=2,length=0.1)
points(x3,y3,pch=19,col="black",cex=.75)
arrows(x3,y3,x3+dx3*c,y3+dy3*c,code=2,length=0.1)
points(x4,y4,pch=19,col="black",cex=.75)
arrows(x4,y4,x4+dx4*c,y4+dy4*c,code=2,length=0.1)
dev.off()
