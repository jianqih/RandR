######################################
### This file generates Figure 9.2
### Wald Statistic as a function of s
######################################


beta1 <- 0.8
beta2 <- 1.6
s <- seq(1,10,0.1)
f1 <- 10*(beta1^s-1)^2/(s^2)/(beta1^(2*s-2))
f2 <- 10*(beta2^s-1)^2/(s^2)/(beta2^(2*s-2))

pdf("HANSEN9-2.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(s,f1,type="l",xlim=c(1,10),ylim=c(0,5),xlab="s",ylab="Wald Statistic",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",cex.lab=.75)
lines(s,f2)
axis(side=1,seq(0,10,2),cex.axis=.75)
axis(side=2,seq(0,5,1),cex.axis=.75)
text(7.9,3,expression(beta==0.8),cex=.8)
text(2.2,3,expression(beta==1.6),cex=.8)
dev.off()

postscript("HANSEN9-2.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(s,f1,type="l",xlim=c(1,10),ylim=c(0,5),xlab="s",ylab="Wald Statistic",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",cex.lab=.75)
lines(s,f2)
axis(side=1,seq(0,10,2),cex.axis=.75)
axis(side=2,seq(0,5,1),cex.axis=.75)
text(7.9,3,expression(beta==0.8),cex=.8)
text(2.2,3,expression(beta==1.6),cex=.8)
dev.off()
