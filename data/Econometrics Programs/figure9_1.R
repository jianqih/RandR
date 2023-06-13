######################################
### This file generates Figure 9.1
### Hypothesis Sets
######################################
### Uses package spatstat
#########################################

library(spatstat)

wd <- 1.4

pdf("HANSEN9-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-5,5),c(-5,5))
W1 <- ellipse(a=1,b=1.1,centre=c(0,0),phi=0,npoly=1024)
W2 <- ellipse(a=4.8,b=5.3,centre=c(0,0),phi=0,npoly=1024)
plot(W1,add=TRUE,lwd=wd)
plot(W2,add=TRUE,lwd=wd)
text(0,0,expression(H[0]),cex=1)
text(3,3,expression(H[1]),cex=1)
dev.off()

postscript("HANSEN9-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-5,5),c(-5,5))
W1 <- ellipse(a=1,b=1.1,centre=c(0,0),phi=0,npoly=1024)
W2 <- ellipse(a=4.8,b=5.3,centre=c(0,0),phi=0,npoly=1024)
plot(W1,add=TRUE,lwd=wd)
plot(W2,add=TRUE,lwd=wd)
text(0,0,expression(H[0]),cex=1)
text(3,3,expression(H[1]),cex=1)
dev.off()

pdf("HANSEN9-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-5,5),c(-5,5))
W2 <- ellipse(a=4.8,b=5.3,centre=c(0,0),phi=0,npoly=1024)
plot(W2,add=TRUE,lwd=wd)
x <- seq(-4.7,4.7,.01)
y <- sin(x)
lines(x,y,lty=1,lwd=wd)
text(0,3,expression(T<c),cex=1)
text(0,-3,expression(T>c),cex=1)
text(4,0,expression(S[0]),cex=1)
text(4,-1.5,expression(S[1]),cex=1)
dev.off()

postscript("HANSEN9-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-5,5),c(-5,5))
W2 <- ellipse(a=4.8,b=5.3,centre=c(0,0),phi=0,npoly=1024)
plot(W2,add=TRUE,lwd=wd)
x <- seq(-4.7,4.7,.01)
y <- sin(x)
lines(x,y,lty=1,lwd=wd)
text(0,3,expression(T<c),cex=1)
text(0,-3,expression(T>c),cex=1)
text(4,0,expression(S[0]),cex=1)
text(4,-1.5,expression(S[1]),cex=1)
dev.off()
