#########################################################################
##  This file generates critical values for KPSS Distributions
#########################################################################
### Uses file figure16_4.txt
#########################################


dat <- read.table("figure16_4.txt")
x <- dat[,1]
f1 <- dat[,2]
f2 <- dat[,3]

pdf("HANSEN16-4.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f1,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",ylim=c(0,16),xlim=c(0,.5),cex.lab=.75) 
axis(side=1,seq(0,.5,.1),cex.axis=.75)
lines(x,f2)
text(.25,2.3,expression(KPSS[1]),cex=.75)
text(.1,11,expression(KPSS[2]),cex=.75)
dev.off()

postscript("HANSEN16-4.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f1,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",yaxt="n",ylim=c(0,16),xlim=c(0,.5),cex.lab=.75) 
axis(side=1,seq(0,.5,.1),cex.axis=.75)
lines(x,f2)
text(.25,2.3,expression(KPSS[1]),cex=.75)
text(.1,11,expression(KPSS[2]),cex=.75)
dev.off()

