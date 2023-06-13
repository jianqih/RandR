#####################################
### Creates figure 23.4
#####################################
### This file imports the file "figure23_4.txt"
#####################################


dat <- read.table("figure23_4.txt")
qq <- dat[,1]
Fs <- dat[,2]
qL <- min(qq)
qU <- max(qq)
Cboot <- as.matrix(read.table("figure23_4_1.txt"))
qvc90 <- quantile(Cboot,.90)
qvc95 <- quantile(Cboot,.95)
qvc99 <- quantile(Cboot,.99)


pdf("HANSEN23-4.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(qq,Fs,type="l",lty=1,xaxs="i",yaxs="i",ylab="Threshold F Statistic",xlab="Threshold Coefficient",ylim=c(0,81),xlim=c(qL,qU),bty="n",xaxt="n",yaxt="n",bty="n",cex.lab=.75)
axis(1,seq(0,.6,.1),cex.axis=.75)
axis(2,seq(0,100,10),cex.axis=.75)
abline(h=qvc99,lty=2)
abline(h=qvc95,lty=5)
abline(h=qvc90,lty=6)
text(.3,78,"Multiplier Bootstrap 99% Critical Value",cex=.75)
text(.47,53,"95%",cex=.75)
text(.47,45,"90%",cex=.75)
text(.27,35,expression(paste(F[n],(gamma))),cex=.75)
text(.24,65,expression(paste(F[n],(hat(gamma)))),cex=.75)
dev.off()

postscript("HANSEN23-4.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(qq,Fs,type="l",lty=1,xaxs="i",yaxs="i",ylab="Threshold F Statistic",xlab="Threshold Coefficient",ylim=c(0,81),xlim=c(qL,qU),bty="n",xaxt="n",yaxt="n",bty="n",cex.lab=.75)
axis(1,seq(0,.6,.1),cex.axis=.75)
axis(2,seq(0,100,10),cex.axis=.75)
abline(h=qvc99,lty=2)
abline(h=qvc95,lty=5)
abline(h=qvc90,lty=6)
text(.3,78,"Multiplier Bootstrap 99% Critical Value",cex=.75)
text(.47,53,"95%",cex=.75)
text(.47,45,"90%",cex=.75)
text(.27,35,expression(paste(F[n],(gamma))),cex=.75)
text(.24,65,expression(paste(F[n],(hat(gamma)))),cex=.75)
dev.off()

