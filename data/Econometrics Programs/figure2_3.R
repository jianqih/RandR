#############################################
### This file generates Figure 2.3
### Mean Log Wage as a Function of Education
#############################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
lnwage <- as.matrix(log(dat[,5]/(dat[,6]*dat[,7])))
wm <- (dat[,11]==1)&(dat[,2]==0)
wf <- (dat[,11]==1)&(dat[,2]==1)

# take log hourly wage for each group
wm_lnwage <- lnwage[wm]
wf_lnwage <- lnwage[wf]
wm_edu <- dat[wm,4]
wf_edu <- dat[wf,4]

edu_dot <- c(4,6,8,9,10,11,12,13,14,16,18,20)

################################################
###  Conditional Mean  
################################################

wm_mean <- vector()
wf_mean <- vector()

for (i in 1:length(edu_dot)){
  wm_mean[i] <- mean(wm_lnwage[wm_edu == edu_dot[i]])
  wf_mean[i] <- mean(wf_lnwage[wf_edu == edu_dot[i]])
}

pdf("HANSEN2-3.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(edu_dot,wm_mean,type="p",cex=.8,pch=19,xlim=c(4,20),ylim=c(2,4),
     xlab="Education (Years)",ylab="Log Dollars per Hour",yaxt="n",xaxt="n",bty="n",cex.lab=.75)
axis(side=1,seq(2,20,2),cex.axis=.75)
axis(side=2,seq(1.5,4,.5),cex.axis=.75)
points(edu_dot,wf_mean,cex=.8, pch=17)
lines(edu_dot,wm_mean)
lines(edu_dot,wf_mean)
text(18,3,"Women",cex=.75)
text(18,3.75,"Men",cex=.75)
dev.off()

postscript("HANSEN2-3.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(edu_dot,wm_mean,type="p",cex=.8,pch=19,xlim=c(4,20),ylim=c(2,4),
     xlab="Education (Years)",ylab="Log Dollars per Hour",yaxt="n",xaxt="n",bty="n",cex.lab=.75)
axis(side=1,seq(2,20,2),cex.axis=.75)
axis(side=2,seq(1.5,4,.5),cex.axis=.75)
points(edu_dot,wf_mean,cex=.8, pch=17)
lines(edu_dot,wm_mean)
lines(edu_dot,wf_mean)
text(18,3,"Women",cex=.75)
text(18,3.75,"Men",cex=.75)
dev.off()