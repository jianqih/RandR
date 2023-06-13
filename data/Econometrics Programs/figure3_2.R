#########################################
### This file generates Figure 3.2
#########################################
### Uses packages plot3D, scatterplot3D, MASS
#########################################

library(plot3D)
library(scatterplot3d)
library(MASS)    

### generate function for sse
n <- 100
set.seed(3)
mu <- c(0,0)
sigma <- matrix(c(1/4,-1/8,-1/8,1/4),2)
x<-mvrnorm(n,mu,sigma)
e <- 2*rnorm(n)
x1 <- x[,1]
x2 <- x[,2]
y <- x1*3 + x2*3 + e

### evaluation point 
foo <- data.frame(x1,x2,y)
fit <- lm(y~x1+x2,data=foo)

grid.lines=5
x1.pred <- seq(min(x1),max(x1),length.out=grid.lines)
x2.pred <- seq(min(x2),max(x2),length.out=grid.lines)
pred <- expand.grid(x1=x1.pred,x2=x2.pred)
y.pred <- matrix(predict(fit,newdata=pred),nrow=grid.lines,ncol=grid.lines)
fitpoints <- predict(fit)

pdf("HANSEN3-2.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
scatter3D(x1,x2,y,colvar=NULL,pch=1,theta=20,phi=0,box=FALSE,
          nticks=10,surf=list(x=x1.pred,y=x2.pred,z=y.pred,facets=NA,fit=fitpoints),cex=.75)
arrows3D(min(x1),min(x2),min(y),max(x1),min(x2),min(y),add=TRUE,col="black")
arrows3D(min(x1),min(x2),min(y),min(x1),max(x2),min(y),add=TRUE,col="black")
arrows3D(min(x1),min(x2),min(y),min(x1),min(x2),max(y),add=TRUE,col="black")
text3D(max(x1)+0.02,min(x2),min(y),labels=expression(X[1]),add=TRUE,cex=.75)
text3D(min(x1),max(x2)+0.02,min(y),labels=expression(X[2]),add=TRUE,cex=.75)
text3D(min(x1),min(x2),max(y)+0.5,labels="Y",add=TRUE,cex=.75)
dev.off()

postscript("HANSEN3-2.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
scatter3D(x1,x2,y,colvar=NULL,pch=1,theta=20,phi=0,box=FALSE,
          nticks=10,surf=list(x=x1.pred,y=x2.pred,z=y.pred,facets=NA,fit=fitpoints),cex=.75)
arrows3D(min(x1),min(x2),min(y),max(x1),min(x2),min(y),add=TRUE,col="black")
arrows3D(min(x1),min(x2),min(y),min(x1),max(x2),min(y),add=TRUE,col="black")
arrows3D(min(x1),min(x2),min(y),min(x1),min(x2),max(y),add=TRUE,col="black")
text3D(max(x1)+0.02,min(x2),min(y),labels=expression(X[1]),add=TRUE,cex=.75)
text3D(min(x1),max(x2)+0.02,min(y),labels=expression(X[2]),add=TRUE,cex=.75)
text3D(min(x1),min(x2),max(y)+0.5,labels="Y",add=TRUE,cex=.75)
dev.off()