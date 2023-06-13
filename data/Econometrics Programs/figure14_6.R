#####################################
### This file generates Figure 14.6
### AR(2) Triangle
####################################


a1 <- seq(-2, 2, by=0.01) 
a2 <- -a1^2/4

pdf("HANSEN14-6.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(a1,a2,lty=1,type="l",xlab=expression(alpha[1]),ylab=expression(alpha[2]),ylim=c(-1.4,1.1),cex.lab=.8,cex.axis=.75,bty="n",xaxt="n")
segments(-2,-1,2,-1)
segments(-2,-1,0,1)
segments(0,1,2,-1)
text(0,-.5,"Complex Factors",cex=.75)
text(0,.3,"Real Factors",cex=.75)
text(-1.3,.5,expression(alpha[2]-alpha[1]==1),cex=.8)
text(1.3,.5,expression(alpha[1]+alpha[2]==1),cex=.8)
text(-.6,-1.25,expression(alpha[2]==-1),cex=.8)
arrows(-1.085,.435,-.8,.27,code=2,length=0.1)
arrows(1.065,.435,.8,.27,code=2,length=0.1)
arrows(-.265,-1.25,0.1,-1.02,code=2,length=0.1)
axis(side=1,seq(-3,3,1),cex.axis=.75)
dev.off()

postscript("HANSEN14-6.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(a1,a2,lty=1,type="l",xlab=expression(alpha[1]),ylab=expression(alpha[2]),ylim=c(-1.4,1.1),cex.lab=.8,cex.axis=.75,bty="n")
segments(-2,-1,2,-1)
segments(-2,-1,0,1)
segments(0,1,2,-1)
text(0,-.5,"Complex Factors",cex=.75)
text(0,.3,"Real Factors",cex=.75)
text(-1.3,.5,expression(alpha[2]-alpha[1]==1),cex=.8)
text(1.3,.5,expression(alpha[1]+alpha[2]==1),cex=.8)
text(-.6,-1.25,expression(alpha[2]==-1),cex=.8)
arrows(-1.085,.435,-.8,.27,code=2,length=0.1)
arrows(1.065,.435,.8,.27,code=2,length=0.1)
arrows(-.265,-1.25,0.1,-1.02,code=2,length=0.1)
axis(side=1,seq(-3,3,1),cex.axis=.75)
dev.off()