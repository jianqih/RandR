#########################################################################
##  This file generates Figure 17.1
##  Fixed Effect Bias
#########################################################################

y <- as.matrix(c(1,2,3,6,7,8,11,12,13))
x <- as.matrix(c(3,2,1,8,7,6,13,12,11))
z <- cbind(matrix(1,9,1),x)
beta <- solve(t(z)%*%z,t(z)%*%y)

xx <- seq(0,15,.01)
f <- beta[1]+xx*beta[2]

x1 <- x[1:3]
y1 <- y[1:3]
x2 <- x[4:6]
y2 <- y[4:6]
x3 <- x[7:9]
y3 <- y[7:9]

pdf("HANSEN17-1.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xx,f,type="l",lty=1,ylab="",xlab="",xaxs="i",yaxs="i",ylim=c(-1.5,15),xlim=c(-1.5,15),xaxt="n",yaxt="n",cex.lab=.75,bty="n",lwd=2)
points(x1,y1,pch=15,cex=1.2)
points(x2,y2,pch=16,cex=1.2)
points(x3,y3,pch=17,cex=1.2)
lines(c(.5,3.5),c(3.5,.5),lwd=1.5)
lines(c(5.5,8.5),c(8.5,5.5),lwd=1.5)
lines(c(10.5,13.5),c(13.5,10.5),lwd=1.5)
lines(c(0,15),c(0,0))
lines(c(0,0),c(0,15))
text(5,12,"Least Squares Fit",cex=.75)
arrows(7.55,11.75,9.5,9.5,angle=20,length=.1)
text(4,2,"Firm 1",cex=.75)
text(9,7,"Firm 2",cex=.75)
text(14,12,"Firm 3",cex=.75)
text(14,-1,"X",cex=.75)
text(-1,14,"Y",cex=.75)
text(12,3,"True Regression Line",cex=.75)
arrows(9,3,3.7,0.6,angle=20,length=.1)
arrows(9.25,3.5,8.6,5.3,angle=20,length=.1)
arrows(9.5,3.5,13.4,10.3,angle=20,length=.1)
dev.off()

postscript("HANSEN17-1.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xx,f,type="l",lty=1,ylab="",xlab="",xaxs="i",yaxs="i",ylim=c(-1.5,15),xlim=c(-1.5,15),xaxt="n",yaxt="n",cex.lab=.75,bty="n",lwd=2)
points(x1,y1,pch=15,cex=1.2)
points(x2,y2,pch=16,cex=1.2)
points(x3,y3,pch=17,cex=1.2)
lines(c(.5,3.5),c(3.5,.5),lwd=1.5)
lines(c(5.5,8.5),c(8.5,5.5),lwd=1.5)
lines(c(10.5,13.5),c(13.5,10.5),lwd=1.5)
lines(c(0,15),c(0,0))
lines(c(0,0),c(0,15))
text(5,12,"Least Squares Fit",cex=.75)
arrows(7.55,11.75,9.5,9.5,angle=20,length=.1)
text(4,2,"Firm 1",cex=.75)
text(9,7,"Firm 2",cex=.75)
text(14,12,"Firm 3",cex=.75)
text(14,-1,"X",cex=.75)
text(-1,14,"Y",cex=.75)
text(12,3,"True Regression Line",cex=.75)
arrows(9,3,3.7,0.6,angle=20,length=.1)
arrows(9.25,3.5,8.6,5.3,angle=20,length=.1)
arrows(9.5,3.5,13.4,10.3,angle=20,length=.1)
dev.off()
