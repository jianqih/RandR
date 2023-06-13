################################################
### This file generates Figure 2.7
### Conditional Mean and Two Linear Projections 
################################################


# draw (x1,y1) and (x2,y2) from DGPs
qq <- function(x)(x*2-(x^2/6))
n <- 10000
x1 <- rnorm(n)+2
y1 <- qq(x1)+rnorm(n)
x2 <- rnorm(n)+4
y2 <- qq(x2)+rnorm(n)

# ols fit 
test <- data.frame(x1 = seq(0,60,0.1))
test$x2 <- test$x1
fit1 <- predict(lm(y1~x1),test)
fit2 <- predict(lm(y2~x2),test)

leg1 <- "Conditional Expectation"
leg2 <- "Linear Projection 1"
leg3 <- "Linear Projection 2"

pdf("HANSEN2-7.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(test$x1,fit1,type="l",lty=1,xaxs="i",yaxs="i",xlim=c(-.01,6),ylim=c(-.01,8),ylab="",xlab="",bty="n",cex.axis=.75,yaxt="n",xaxt="n")
lines(c(0,6),c(0,0))
lines(c(0,0),c(0,8))
lines(test$x1,fit2,type="l")
lines(test$x1,qq(test$x1),type="l",lwd=2)
text(4.2,3,leg1,cex=.75)
text(1.3,4.5,leg2,cex=.75)
text(3.2,7,leg3,cex=.75)
arrows(2.75,3.1,2.2,3.55,angle=20,length=.1)
arrows(5.25,3.3,5.6,5.9,angle=20,length=.1)
arrows(4.45,7,4.8,6.9,angle=20,length=.1)
arrows(1.5,4.2,1.7,3.65,angle=20,length=.1)
title(xlab="Regressor",line=1,cex.lab=.8)
title(ylab="Dependent Variable",line=1,cex.lab=.8)
dev.off()

postscript("HANSEN2-7.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(test$x1,fit1,type="l",lty=1,xaxs="i",yaxs="i",xlim=c(-.01,6),ylim=c(-.01,8),ylab="",xlab="",bty="n",cex.axis=.75,yaxt="n",xaxt="n")
lines(c(0,6),c(0,0))
lines(c(0,0),c(0,8))
lines(test$x1,fit2,type="l")
lines(test$x1,qq(test$x1),type="l",lwd=2)
text(4.2,3,leg1,cex=.75)
text(1.3,4.5,leg2,cex=.75)
text(3.2,7,leg3,cex=.75)
arrows(2.75,3.1,2.2,3.55,angle=20,length=.1)
arrows(5.25,3.3,5.6,5.9,angle=20,length=.1)
arrows(4.45,7,4.8,6.9,angle=20,length=.1)
arrows(1.5,4.2,1.7,3.65,angle=20,length=.1)
title(xlab="Regressor",line=1,cex.lab=.8)
title(ylab="Dependent Variable",line=1,cex.lab=.8)
dev.off()
