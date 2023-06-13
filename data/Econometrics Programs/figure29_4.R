#####################################
### This file generates Figure 29.4
### Thresholding
#####################################

x <- (-180:180)/100
ridge <- x*.7
select <- x*(abs(x)>1)
lasso <- (x-1)*(x>1) + (x+1)*(-x>1)

wd <- 1.4

pdf("HANSEN29-4a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-2,2),c(-2,2))
arrows(0,0,2,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,-2,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,2,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,-2,angle=20,length=.1,col="grey",lwd=wd)
lines(x,x,lty=2,lwd=wd)
lines(x,select,lwd=wd)
lines(x,ridge,lty=5,lwd=wd)
text(.5,.9,expression(hat(beta)[ols]))
text(1.3,.35,expression(hat(beta)[select]))
text(1.8,.85,expression(hat(beta)[ridge]))
dev.off()

postscript("HANSEN29-4a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-2,2),c(-2,2))
arrows(0,0,2,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,-2,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,2,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,-2,angle=20,length=.1,col="grey",lwd=wd)
lines(x,x,lty=2,lwd=wd)
lines(x,select,lwd=wd)
lines(x,ridge,lty=5,lwd=wd)
text(.5,.9,expression(hat(beta)[ols]))
text(1.3,.35,expression(hat(beta)[select]))
text(1.8,.85,expression(hat(beta)[ridge]))
dev.off()


pdf("HANSEN29-4b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-2,2),c(-2,2))
arrows(0,0,2,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,-2,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,2,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,-2,angle=20,length=.1,col="grey",lwd=wd)
lines(x,x,lty=5,lwd=wd)
lines(x,lasso,lwd=wd)
text(1.35,1.7,expression(hat(beta)[ols]))
text(1.05,.5,expression(hat(beta)[Lasso]))
dev.off()

postscript("HANSEN29-4b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-2,2),c(-2,2))
arrows(0,0,2,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,-2,0,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,2,angle=20,length=.1,col="grey",lwd=wd)
arrows(0,0,0,-2,angle=20,length=.1,col="grey",lwd=wd)
lines(x,x,lty=5,lwd=wd)
lines(x,lasso,lwd=wd)
text(1.35,1.7,expression(hat(beta)[ols]))
text(1.05,.5,expression(hat(beta)[Lasso]))
dev.off()
