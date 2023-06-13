#####################################
### This file generates Figure 4.1
#####################################
### Uses package statstat
#########################################

library(spatstat)

x <- seq(-1,1,.01)
f1 <- (1 - (x^2))*.75
f2 <- f1*(1+x)
leg1 <- expression(f(y))
leg2 <- expression(f[beta](y))

pdf("HANSEN4-1.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f1,type="l",lty=1,xaxs="i",yaxs="i",xaxt="n",xlim=c(-1,1),ylim=c(-0.075,0.9),ylab="",xlab="",yaxt="n",bty="n")
abline(h=0)
lines(x,f2)
arrows(0,.75,0,0,angle=20,length=.1)
arrows(.2,108/125,.2,0,angle=20,length=.1)
text(0,-.05,expression(beta[0]),cex=.8)
text(.2,-.05,expression(beta),cex=.8)
arrows(0,.75,0,0,angle=20,length=.1)
arrows(.2,108/125,.2,0,angle=20,length=.1)
text(-.65,.6,leg1,cex=.8)
text(.85,.7,leg2,cex=.8)
dev.off()


postscript("HANSEN4-1.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f1,type="l",lty=1,xaxs="i",yaxs="i",xaxt="n",xlim=c(-1,1),ylim=c(-0.075,0.9),ylab="",xlab="",yaxt="n",bty="n")
abline(h=0)
lines(x,f2)
arrows(0,.75,0,0,angle=20,length=.1)
arrows(.2,108/125,.2,0,angle=20,length=.1)
text(0,-.05,expression(beta[0]),cex=.8)
text(.2,-.05,expression(beta),cex=.8)
arrows(0,.75,0,0,angle=20,length=.1)
arrows(.2,108/125,.2,0,angle=20,length=.1)
text(-.65,.6,leg1,cex=.8)
text(.85,.7,leg2,cex=.8)
dev.off()
