#########################################################################
##  This file generates Figure 2.8
##  Causal Effect
#########################################################################


x <- c(1,2)
ace <- c(9,16)
ls <- c(8.75,17)

pdf("HANSEN2-8.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,ace,type="l",lty=1,ylab="Wage",xlab="",xaxs="i",yaxs="i",ylim=c(7,21),xlim=c(.7,2.4),xaxt="n",yaxt="n",cex.lab=.75,bty="n")
lines(x,ls)
axis(side=2,seq(6,22,2),cex.axis=.75)
axis(side=1,at=seq(0,3,1),labels=c("a","High School","College","b"),cex.axis=.75)
points(1,8,pch=19,cex=1.2)
points(1,10,pch=19,cex=0.7)
points(2,12,pch=19,cex=0.7)
points(2,20,pch=19,cex=1.2)
text(2.1,14.5,"Av. Causal Effect",cex=.75)
text(1.7,16.5,"Regression",cex=.75)
dev.off()

postscript("HANSEN2-8.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,ace,type="l",lty=1,ylab="Wage",xlab="",xaxs="i",yaxs="i",ylim=c(7,21),xlim=c(.7,2.4),xaxt="n",yaxt="n",cex.lab=.75,bty="n")
lines(x,ls)
axis(side=2,seq(6,22,2),cex.axis=.75)
axis(side=1,at=seq(0,3,1),labels=c("a","High School","College","b"),cex.axis=.75)
points(1,8,pch=19,cex=1.2)
points(1,10,pch=19,cex=0.7)
points(2,12,pch=19,cex=0.7)
points(2,20,pch=19,cex=1.2)
text(2.1,14.5,"Av. Causal Effect",cex=.75)
text(1.7,16.5,"Regression",cex=.75)
dev.off()
