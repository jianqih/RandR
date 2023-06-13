#########################################################################
##  This file generates Figure 19.2
##  Asymptotic Smoothing Bias
#########################################################################


x <- seq(0,10,.01)
xn <- length(x)
xi <- x-2
i0 <- (xi == 0)
xi <- xi + i0
i1 <- 1-i0
a <- pi/4
xp <- xi*a
sx = sin(xp)
cx = cos(xp)
m <- (sx/xp)*i1 + i0
d <- ((-(sx/xp) - 2*cx/(xp^2) + 2*sx/(xp^3))*i1 - i0/3)*(a^2)

h1 <- 1/2
h2 <- 1
h3 <- 3/2
m1 <- m + (d/2)*(h1^2)
m2 <- m + (d/2)*(h2^2)
m3 <- m + (d/2)*(h3^2)

pdf("HANSEN19-2.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,m,type="l",lty=1,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",xlim=c(0,10),ylim=c(-.75,1.5),xaxt="n", yaxt="n",bty="n",cex.lab=.75)
lines(x,m1,lty=2)
lines(x,m2,lty=5)
lines(x,m3,lty=6)
axis(side=1,seq(0,10,1),cex.axis=.75)
axis(side=2,seq(-1,2,.5),cex.axis=.75)
legend("topright",legend=c(expression(m(x)),expression(h==1/2),expression(h==1),expression(h==3/2)),lty=c(1,2,5,6),bty="n",cex=.75)
dev.off()

postscript("HANSEN19-2.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,m,type="l",lty=1,ylab="Dependent Variable",xlab="Regressor",xaxs="i",yaxs="i",xlim=c(0,10),ylim=c(-.75,1.5),xaxt="n", yaxt="n",bty="n",cex.lab=.75)
lines(x,m1,lty=2)
lines(x,m2,lty=5)
lines(x,m3,lty=6)
axis(side=1,seq(0,10,1),cex.axis=.75)
axis(side=2,seq(-1,2,.5),cex.axis=.75)
legend("topright",legend=c(expression(m(x)),expression(h==1/2),expression(h==1),expression(h==3/2)),lty=c(1,2,5,6),bty="n",cex=.75)
dev.off()


