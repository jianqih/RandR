#####################################
### This file generates Figure 28.2
### Coverage probability of PMS CIs
#####################################
### Uses file figure28_2.txt
#####################################


dat <- read.table("figure28_2.txt")
bs <- dat[,1]
cp <- dat[,2:5]

ylabel <- expression(paste("Coverage Probability for  ",beta[1]))
leg1 <- expression(rho==0.0)
leg2 <- expression(rho==0.3)
leg3 <- expression(rho==0.5)
leg4 <- expression(rho==0.7)
leg5 <- expression(rho==0.8)

pdf("HANSEN28-2.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(bs,cp[,1],type="l",lty=1,xaxs="i",yaxs="i",ylab=ylabel,xlab=expression(beta[2]),bty="n",cex.lab=.8,ylim=c(.6,1),cex.axis=.75)
abline(h=0.95,lty=1)
lines(bs,cp[,2],lty=1)
lines(bs,cp[,3],lty=1)
lines(bs,cp[,4],lty=1)
text(.4,.96,leg1,cex=.8)
text(.5,.92,leg2,cex=.8)
text(.6,.87,leg3,cex=.8)
text(.7,.76,leg4,cex=.8)
text(.83,.68,leg5,cex=.8)
dev.off()

postscript("HANSEN28-2.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(bs,cp[,1],type="l",lty=1,xaxs="i",yaxs="i",ylab=ylabel,xlab=expression(beta[2]),bty="n",cex.lab=.8,ylim=c(.6,1),cex.axis=.75)
abline(h=0.95,lty=1)
lines(bs,cp[,2],lty=1)
lines(bs,cp[,3],lty=1)
lines(bs,cp[,4],lty=1)
text(.4,.96,leg1,cex=.8)
text(.5,.92,leg2,cex=.8)
text(.6,.87,leg3,cex=.8)
text(.7,.76,leg4,cex=.8)
text(.83,.68,leg5,cex=.8)
dev.off()

