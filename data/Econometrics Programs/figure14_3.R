#####################################
### This file generates Figures 14.3, 14.4, 14.5, 14.7
### Plots of MA and AR processes
#####################################


n <- 120
set.seed(56)
e  <- ts(as.matrix(rnorm(n)),frequency=4,start=c(1985,1))
e2 <- ts(as.matrix(rnorm(n)),frequency=4,start=c(1985,1))

y1 <- e
y2 <- e + lag(e,-1) + lag(e,-2) + lag(e,-3) + lag(e,-4) + lag(e,-5) + lag(e,-6) + lag(e,-7) + lag(e,-8)
y3 <- e
y4 <- e
y5 <- e
y6 <- e2
y7 <- e
y8 <- e
for (t in 2:n){
 y3[t] <- y3[t-1]*.5 + e[t]
 y4[t] <- y4[t-1]*.95 + e[t]
 y5[t] <- y5[t-1] + e[t]
 y6[t] <- y6[t-1] + e2[t]
}
for (t in 3:n){
 y7[t] <- y7[t-1]*.4 + y7[t-2]*.4 + e[t]
 y8[t] <- y8[t-1]*1.3 - y8[t-2]*.8 + e[t]
}


wd <- 1.4

pdf("HANSEN14-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(y1,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-4,2,1),lwd=wd)
dev.off()

postscript("HANSEN14-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(y1,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-4,2,1),lwd=wd)
dev.off()

pdf("HANSEN14-3b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(y2,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-12,8,4),lwd=wd)
dev.off()

postscript("HANSEN14-3b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(y2,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-12,8,4),lwd=wd)
dev.off()

pdf("HANSEN14-4a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(y3,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-4,3,1),lwd=wd)
dev.off()

postscript("HANSEN14-4a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(y3,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-4,3,1),lwd=wd)
dev.off()

pdf("HANSEN14-4b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(y4,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-8,6,2),lwd=wd)
dev.off()

postscript("HANSEN14-4b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(y4,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-8,6,2),lwd=wd)
dev.off()

pdf("HANSEN14-5a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(y5,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
dev.off()

postscript("HANSEN14-5a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(y5,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-12,12,4),lwd=wd)
dev.off()

pdf("HANSEN14-5b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(y6,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-40,10,10),lwd=wd)
dev.off()

postscript("HANSEN14-5b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(y6,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-40,10,10),lwd=wd)
dev.off()

pdf("HANSEN14-7a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(y7,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-10,10,1),lwd=wd)
dev.off()

postscript("HANSEN14-7a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(y7,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-10,10,1),lwd=wd)
dev.off()

pdf("HANSEN14-7b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.ts(y8,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-6,6,2),lwd=wd)
dev.off()

postscript("HANSEN14-7b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.ts(y8,xlab="",ylab="",bty="n",xaxt="n",yaxt="n",lwd=wd,xlim=c(1987,2015))
axis(side=1,seq(1980,2020,5),lwd=wd)
axis(side=2,seq(-6,6,2),lwd=wd)
dev.off()
