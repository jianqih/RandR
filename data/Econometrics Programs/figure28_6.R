#####################################
### This file generates Figure 28.6
### Probability Simplex in 2D and 3D
#####################################
### Uses package plot3D
#####################################

library(plot3D)

wd <- 1.4

pdf("HANSEN28-6a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-.2,1.8),c(-.1,1.5))
arrows(0,0,1.3,0,angle=20,length=.1,lwd=wd)
arrows(0,0,0,1.3,angle=20,length=.1,lwd=wd)
segments(1,0,0,1,col="black",lwd=wd)
text(1,-.1,"(1,0)")
text(-.15,1,"(0,1)")
text(1.4,-.05,expression(w[1]))
text(-.1,1.3,expression(w[2]))
points(.7,.3,pch=19,col="black",cex=.7)
text(.95,.35,"w=(.7,.3)")
dev.off()

postscript("HANSEN28-6a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-.2,1.8),c(-.1,1.5))
arrows(0,0,1.3,0,angle=20,length=.1,lwd=wd)
arrows(0,0,0,1.3,angle=20,length=.1,lwd=wd)
segments(1,0,0,1,col="black",lwd=wd)
text(1,-.1,"(1,0)")
text(-.15,1,"(0,1)")
text(1.4,-.05,expression(w[1]))
text(-.1,1.3,expression(w[2]))
points(.7,.3,pch=19,col="black",cex=.7)
text(.95,.35,"w=(.7,.3)")
dev.off()


pdf("HANSEN28-6b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-2,2.5),c(-2,2))
polygon(c(-sqrt(3)/2,sqrt(3)/2,0,-sqrt(3)/2),c(-1/2,-1/2,1,-1/2),col=gray(.8),lwd=wd)
arrows(0,0,0,2,angle=20,length=.1,lwd=wd)
arrows(0,0,sqrt(3),-1,angle=20,length=.1,lwd=wd)
arrows(0,0,-sqrt(3),-1,angle=20,length=.1,lwd=wd)
text(-1.4,-1.1,expression(w[1]))
text(1.45,-1.1,expression(w[2]))
text(.2,2,expression(w[3]))
text(1.3,-.4,"(1,0,0)")
text(0.45,1.05,"(0,1,0)")
text(-1.3,-.4,"(0,0,1)")
points(.3,.25,pch=19,col="black",cex=.7)
text(1.05,.35,"w=(.1,.5,.4)")
dev.off()

postscript("HANSEN28-6b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-2,2.5),c(-2,2))
polygon(c(-sqrt(3)/2,sqrt(3)/2,0,-sqrt(3)/2),c(-1/2,-1/2,1,-1/2),col=gray(.8),lwd=wd)
arrows(0,0,0,2,angle=20,length=.1,lwd=wd)
arrows(0,0,sqrt(3),-1,angle=20,length=.1,lwd=wd)
arrows(0,0,-sqrt(3),-1,angle=20,length=.1,lwd=wd)
text(-1.4,-1.1,expression(w[1]))
text(1.45,-1.1,expression(w[2]))
text(.2,2,expression(w[3]))
text(1.3,-.4,"(1,0,0)")
text(0.45,1.05,"(0,1,0)")
text(-1.3,-.4,"(0,0,1)")
points(.3,.25,pch=19,col="black",cex=.7)
text(1.05,.35,"w=(.1,.5,.4)")
dev.off()