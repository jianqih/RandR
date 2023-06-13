#########################################
### This file generates Figure 3.4
#########################################
### Uses package plot3D
#########################################

library(plot3D)

pdf("HANSEN3-4.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
arrows3D(0,0,0,0.3,0.3,0.7,col="black",xlim=c(0,1.06),ylim=c(0,1.06),zlim=c(0,0.74),theta=20,phi=10,box=FALSE)
polygon3D(c(0,1,0),c(0,0,1),c(0,0,0),add=TRUE,col=gray(.8))
arrows3D(0,0,0,0.3,0.3,0.7,add=TRUE,col="black")
arrows3D(0,0,0,1,0,0,add=TRUE,col="black")
arrows3D(0,0,0,0,1,0,add=TRUE,col="black")
text3D(1.02,0,0,labels=expression(X[1]),add=TRUE,col="black",cex=.75)
text3D(0,1.05,0,labels=expression(X[2]),add=TRUE,col="black",cex=.75)
text3D(0.3,0.3,0.73,labels="Y",add=TRUE,col="black",cex=.75)
text3D(0.3,0.33,0.4,labels=expression(hat(e)),add=TRUE,col="black",cex=.75)
segments3D(0.3,0.3,0.7,0.3,0.3,0,lty=2,add=TRUE,col="black")
segments3D(0,0,0,0.3,0.3,0,lty=2,add=TRUE,col="black")
text3D(0.3,0.3,-0.05,labels=expression(hat(Y)),add=TRUE,col="black",cex=.75)
segments3D(0.27,0.27,0.03,0.3,0.3,0.03,add=TRUE,col="black")
segments3D(0.27,0.27,0,0.27,0.27,0.03,add=TRUE,col="black")
dev.off()

postscript("HANSEN3-4.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
arrows3D(0,0,0,0.3,0.3,0.7,col="black",xlim=c(0,1.06),ylim=c(0,1.06),zlim=c(0,0.74),theta=20,phi=10,box=FALSE)
polygon3D(c(0,1,0),c(0,0,1),c(0,0,0),add=TRUE,col=gray(.8))
arrows3D(0,0,0,0.3,0.3,0.7,add=TRUE,col="black")
arrows3D(0,0,0,1,0,0,add=TRUE,col="black")
arrows3D(0,0,0,0,1,0,add=TRUE,col="black")
text3D(1.02,0,0,labels=expression(X[1]),add=TRUE,col="black",cex=.75)
text3D(0,1.05,0,labels=expression(X[2]),add=TRUE,col="black",cex=.75)
text3D(0.3,0.3,0.73,labels="Y",add=TRUE,col="black",cex=.75)
text3D(0.3,0.33,0.4,labels=expression(hat(e)),add=TRUE,col="black",cex=.75)
segments3D(0.3,0.3,0.7,0.3,0.3,0,lty=2,add=TRUE,col="black")
segments3D(0,0,0,0.3,0.3,0,lty=2,add=TRUE,col="black")
text3D(0.3,0.3,-0.05,labels=expression(hat(Y)),add=TRUE,col="black",cex=.75)
segments3D(0.27,0.27,0.03,0.3,0.3,0.03,add=TRUE,col="black")
segments3D(0.27,0.27,0,0.27,0.27,0.03,add=TRUE,col="black")
dev.off()
