#################################################
### This file generates Figure 7.3a
### Contours of Joint Distribution of 
### (beta_1_hat,beta_2_hat), homoskedastic case 
#################################################


rho <- 0.5
sr <- 1 - rho^2
x <- seq(from=-3,by=0.01,length.out=701)
fun2 <- function(x,y){z<-(x^2+2*rho*y*x+y^2)/sr}
f <- outer(x,x,Vectorize(fun2))
cdf_f <- pchisq(f,2)

wd <- 1.4

pdf("HANSEN7-3a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)	
oldpar <- par(mar=c(5.1,5.1,4.1,2.1))
contour(x,x,cdf_f,ylim=c(-3,3),xlim=c(-3,3), zlim=c(0,1), nlevels=9, 
        drawlabels=FALSE,xlab=expression(paste(beta[1])),ylab=expression(paste(beta[2])),
        xaxs="i",yaxs="i",bty="n",cex.lab=1.0,lwd=wd)
axis(1,lwd=wd)
axis(2,lwd=wd)	
par(oldpar)
dev.off()

postscript("HANSEN7-3a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
oldpar <- par(mar=c(5.1,5.1,4.1,2.1))
contour(x,x,cdf_f,ylim=c(-3,3),xlim=c(-3,3), zlim=c(0,1), nlevels=9, 
        drawlabels=FALSE,xlab=expression(paste(beta[1])),ylab=expression(paste(beta[2])),
        xaxs="i",yaxs="i",bty="n",cex.lab=1.0,lwd=wd)
axis(1,lwd=wd)
axis(2,lwd=wd)	
par(oldpar)
dev.off()