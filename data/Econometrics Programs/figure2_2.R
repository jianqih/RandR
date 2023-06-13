#####################################
### This file generates Figure 2.2
### Log Wage Density by Gender and Race
#####################################
### Uses data file cps09mar.txt
#########################################

dat <- read.table("cps09mar.txt")
f <- (dat[,2]==1)
m <- (dat[,2]==0)
wm <- (dat[,11]==1)&m
bm <- (dat[,11]==2)&m
wf <- (dat[,11]==1)&f
bf <- (dat[,11]==2)&f
lnwage <- as.matrix(log(dat[,5]/(dat[,6]*dat[,7])))

# take log hourly wage for each group
f_lnwage  <- lnwage[f]
m_lnwage  <- lnwage[m]
wm_lnwage <- lnwage[wm]
bm_lnwage <- lnwage[bm]
wf_lnwage <- lnwage[wf]
bf_lnwage <- lnwage[bf]

##########################################
### density with bandwidth = 0.075 (gender)
##########################################

f_den <- density(f_lnwage,from=0,to=6,adjust=2)		
m_den <- density(m_lnwage,from=0,to=6,adjust=2)		

# mark the point of the mean wage
medm <- min(which( abs(m_den$x-mean((m_lnwage))) <= 0.015 ))
xm <- m_den$x[medm]
medf <- min(which( abs(f_den$x-mean((f_lnwage))) <= 0.01 ))
xf <- f_den$x[medf]

wd <- 1.4

pdf("HANSEN2-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(m_den$x, m_den$y, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,6),ylim=c(0,0.75),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
lines(f_den,lwd=wd)
axis(side=1,seq(0,10,1),lwd=wd)
text(4.3,0.4,"Men")
text(1.5,0.4,"Women")
dev.off()

postscript("HANSEN2-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(m_den$x, m_den$y, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,6),ylim=c(0,0.75),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
lines(f_den,lwd=wd)
axis(side=1,seq(0,10,1),lwd=wd)
text(4.3,0.4,"Men")
text(1.5,0.4,"Women")
dev.off()




bf_den <- density(bf_lnwage,from=0,to=6,adjust=2)		
bm_den <- density(bm_lnwage,from=0,to=6,adjust=2)		
wf_den <- density(wf_lnwage,from=0,to=6,adjust=2)		
wm_den <- density(wm_lnwage,from=0,to=6,adjust=2)

leg1 <- "White Men"
leg2 <- "Black Men"
leg3 <- "White Women"
leg4 <- "Black Women"


pdf("HANSEN2-2b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(wm_den$x, wm_den$y, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(1,5),ylim=c(0,0.85),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
lines(wf_den,lwd=wd)
lines(bm_den,lwd=wd)
lines(bf_den,lwd=wd)
text(4.4,0.3,leg1)
text(4.2,0.5,leg2)
text(1.75,0.7,leg3)
text(1.65,0.55,leg4)
axis(side=1,seq(-1,7,1),lwd=wd)
arrows(4.3,.27,4.0,0.22,angle=20,length=.1,lwd=wd)
arrows(4.0,.47,3.5,0.41,angle=20,length=.1,lwd=wd)
arrows(1.8,.52,2.14,0.42,angle=20,length=.1,lwd=wd)
arrows(2.1,.67,2.48,0.60,angle=20,length=.1,lwd=wd)
dev.off()

postscript("HANSEN2-2b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(wm_den$x, wm_den$y, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(1,5),ylim=c(0,0.85),title=NULL,xaxs="i",yaxs="i",bty="n",lwd=wd)
lines(wf_den,lwd=wd)
lines(bm_den,lwd=wd)
lines(bf_den,lwd=wd)
text(4.4,0.3,leg1)
text(4.2,0.5,leg2)
text(1.75,0.7,leg3)
text(1.65,0.55,leg4)
axis(side=1,seq(-1,7,1),lwd=wd)
arrows(4.3,.27,4.0,0.22,angle=20,length=.1,lwd=wd)
arrows(4.0,.47,3.5,0.41,angle=20,length=.1,lwd=wd)
arrows(1.8,.52,2.14,0.42,angle=20,length=.1,lwd=wd)
arrows(2.1,.67,2.48,0.60,angle=20,length=.1,lwd=wd)
dev.off()

