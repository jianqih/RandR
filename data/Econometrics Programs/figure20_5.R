#####################################
### This file generates Figure 20.5
### ISE
#####################################

kmax <- 8
n1 <- 10
n2 <- 30
n3 <- 150
K <- (1:kmax)
m1 <- 1/(K^4) + K/n1
m2 <- 1/(K^4) + K/n2
m3 <- 1/(K^4) + K/n3
K1 <- which.min(m1)
K2 <- which.min(m2)
K3 <- which.min(m3)
M1 <- min(m1)
M2 <- min(m2)
M3 <- min(m3)

pdf("HANSEN20-5.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(K,m3,lty=1,xaxs="i",yaxs="i",type="l",ylab="Integrated Squared Error",xlab="Series Order",ylim=c(0,1),bty="n",cex.lab=.75,xaxt="n",yaxt="n")
axis(side=1,cex.axis=.75)
axis(side=2,cex.axis=.75)
lines(K,m2,lty=2)
lines(K,m1,lty=5)
points(K1,M1,pch=19,cex=.6)
points(K2,M2,pch=19,cex=.6)
points(K3,M3,pch=19,cex=.6)
text(5,.61,expression(K^-4 +K/10),cex=.75)
text(6,.26,expression(K^-4 +K/30),cex=.75)
text(7,.09,expression(K^-4 +K/150),cex=.75)
dev.off()

postscript("HANSEN20-5.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(K,m3,lty=1,xaxs="i",yaxs="i",type="l",ylab="Integrated Squared Error",xlab="Series Order",ylim=c(0,1),bty="n",cex.lab=.75,xaxt="n",yaxt="n")
axis(side=1,cex.axis=.75)
axis(side=2,cex.axis=.75)
lines(K,m2,lty=2)
lines(K,m1,lty=5)
points(K1,M1,pch=19,cex=.6)
points(K2,M2,pch=19,cex=.6)
points(K3,M3,pch=19,cex=.6)
text(5,.61,expression(K^-4 +K/10),cex=.75)
text(6,.26,expression(K^-4 +K/30),cex=.75)
text(7,.09,expression(K^-4 +K/150),cex=.75)
dev.off()