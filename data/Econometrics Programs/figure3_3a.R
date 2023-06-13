#########################################
### This file generates Figure 3.3a,
### Sum-of-Squared Errors Function
### as a function of beta1,beta2
#########################################
### Uses packages lattice, MASS
#########################################

library(lattice) # package for 3D-plot
library(MASS)    # package for mvrnorm 

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

sse <- function (b1,b2) {
  yfit <- x1*b1+x2*b2
  val <- sum((y-yfit)^2)
  return(val)
}

v_sse <- Vectorize(sse)

### evaluation point 

b1 <- seq(2,4,0.05)
b2 <- seq(2,4,0.05)
foo <- data.frame(b1=rep(b1,each=length(b1)),b2=rep(b2,length(b2)))
foo$sse_b <- v_sse(foo$b1,foo$b2)

pdf("HANSEN3-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
print(wireframe(sse_b~b1*b2,foo,xlab=expression(beta[2]),ylab=expression(beta[1]),zlab="",
                scales=list(arrows=FALSE, col="black"),screen=list(z=50,x=-80,y=0),
                par.settings = list(axis.line = list(col = 'transparent'),box.3d = list(col=c("black","black",NA,NA,NA,NA,"black",NA,"black")))))
dev.off()

postscript("HANSEN3-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
print(wireframe(sse_b~b1*b2,foo,xlab=expression(beta[2]),ylab=expression(beta[1]),zlab="",
                scales=list(arrows=FALSE, col="black"),screen=list(z=50,x=-80,y=0),
                 par.settings = list(axis.line = list(col = 'transparent'),box.3d = list(col=c("black","black",NA,NA,NA,NA,"black",NA,"black")))))
dev.off()
