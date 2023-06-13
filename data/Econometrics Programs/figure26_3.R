#####################################
### This file generates Figure 26.3
### Ordered Choice
#####################################
### Uses package pBrackets
#####################################

library(pBrackets)

x <- c(seq(-3,3,.01))
n <- length(x)
F <- pnorm(x)

a <- c(-1.2,-.5,.25,1)
b <- pnorm(a)

pdf("HANSEN26-3.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,F, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(-4,3),ylim=c(0,1),title=NULL,xaxs="i",yaxs="i",bty="n")
par(xpd=NA)
lines(c(-3,-3),c(0,1))
lines(c(-3,3),c(0,0))
lines(c(-3,3),c(1,1))
for (i in 1:4){
lines(c(a[i],a[i]),c(0,b[i]))
lines(c(-3,a[i]),c(b[i],b[i]))
}
text(a[1],-.05,expression(alpha[1]),cex=.75)
text(a[2],-.05,expression(alpha[2]),cex=.75)
text(a[3],-.05,expression(alpha[3]),cex=.75)
text(a[4],-.05,expression(alpha[4]),cex=.75)
text(2.8,-.05,"U*",cex=.75)
t <- 0.005
xt <- -3.04
brackets(xt,t,xt,b[1]-t,h=.15)
brackets(xt,b[1]+t,xt,b[2]-t,h=.15)
brackets(xt,b[2]+t,xt,b[3]-t,h=.15)
brackets(xt,b[3]+t,xt,b[4]-t,h=.15)
brackets(xt,b[4]+t,xt,1-t,h=.15)
text(-3.8,b[1]/2,"P[Y=1]",cex=.75)
text(-3.8,(b[1]+b[2])/2,"P[Y=2]",cex=.75)
text(-3.8,(b[2]+b[3])/2,"P[Y=3]",cex=.75)
text(-3.8,(b[3]+b[4])/2,"P[Y=4]",cex=.75)
text(-3.8,(b[4]+1)/2,"P[Y=5]",cex=.75)
text(2,.9,expression(G(epsilon)),cex=.75)
dev.off()

postscript("HANSEN26-3.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,F, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(-4,3),ylim=c(0,1),title=NULL,xaxs="i",yaxs="i",bty="n")
par(xpd=NA)
lines(c(-3,-3),c(0,1))
lines(c(-3,3),c(0,0))
lines(c(-3,3),c(1,1))
for (i in 1:4){
lines(c(a[i],a[i]),c(0,b[i]))
lines(c(-3,a[i]),c(b[i],b[i]))
}
text(a[1],-.05,expression(alpha[1]),cex=.75)
text(a[2],-.05,expression(alpha[2]),cex=.75)
text(a[3],-.05,expression(alpha[3]),cex=.75)
text(a[4],-.05,expression(alpha[4]),cex=.75)
text(2.8,-.05,"U*",cex=.75)
t <- 0.005
xt <- -3.04
brackets(xt,t,xt,b[1]-t,h=.15)
brackets(xt,b[1]+t,xt,b[2]-t,h=.15)
brackets(xt,b[2]+t,xt,b[3]-t,h=.15)
brackets(xt,b[3]+t,xt,b[4]-t,h=.15)
brackets(xt,b[4]+t,xt,1-t,h=.15)
text(-3.8,b[1]/2,"P[Y=1]",cex=.75)
text(-3.8,(b[1]+b[2])/2,"P[Y=2]",cex=.75)
text(-3.8,(b[2]+b[3])/2,"P[Y=3]",cex=.75)
text(-3.8,(b[3]+b[4])/2,"P[Y=4]",cex=.75)
text(-3.8,(b[4]+1)/2,"P[Y=5]",cex=.75)
text(2,.9,expression(G(epsilon)),cex=.75)
dev.off()
