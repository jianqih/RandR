#########################################################################
##  This file generates Figures 3.1 
##  
#########################################################################

n <- 20
set.seed(3)
xdat <- runif(n,0,1)
e <- rnorm(n)
ydat <- xdat*3 + e
i <- order(xdat)
xdat <- xdat[i]
ydat <- ydat[i]
zdat <- cbind(xdat,matrix(1,n,1))
beta <- solve(t(zdat)%*%zdat,t(zdat)%*%ydat)
dat <- cbind(xdat,ydat,beta[1]*xdat+beta[2])

# Calculate sse(beta) for demeaned Y
y_demean <-ydat-beta[2]
beta_seq <- seq(from=2,to=4,by=0.005)
sse <- function(b){
  resi <- y_demean - xdat*b
  resi_2 <- sum(resi^2)
  return(resi_2)
}
sse_beta <- lapply(beta_seq,sse)
sse_min <- min(unlist(sse_beta))
beta_min <- beta_seq[which.min(unlist(sse_beta))]

wd <- 1.4


pdf("HANSEN3-1a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xdat,ydat,type="p",lty=1,ylab="",xlab="",xaxs="i",yaxs="i",ylim=c(-1.3,3.5),xlim=c(-.2,1),pch=1,xaxt="n", yaxt="n",bty="n",lwd=wd)
apply(as.matrix(dat),1,function(x) segments(x[1],x[2],x[1],x[3],lty=1))
lines(c(0,1),c(beta[2],beta[1]+beta[2]),lwd=wd)
lines(c(0,1),c(-1,-1),lwd=wd)
lines(c(0,0),c(-1,3.5),lwd=wd)
text(.95,-1.2,"X")
text(-.05,3.4,"Y")
text(-.05,beta[2],"0")
dev.off()

postscript("HANSEN3-1a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xdat,ydat,type="p",lty=1,ylab="",xlab="",xaxs="i",yaxs="i",ylim=c(-1.3,3.5),xlim=c(-.2,1),pch=1,xaxt="n", yaxt="n",bty="n",lwd=wd)
apply(as.matrix(dat),1,function(x) segments(x[1],x[2],x[1],x[3],lty=1))
lines(c(0,1),c(beta[2],beta[1]+beta[2]),lwd=wd)
lines(c(0,1),c(-1,-1),lwd=wd)
lines(c(0,0),c(-1,3.5),lwd=wd)
text(.95,-1.2,"X")
text(-.05,3.4,"Y")
text(-.05,beta[2],"0")
dev.off()


pdf("HANSEN3-1b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(beta_seq,sse_beta,type="l",lty=1,ylab="Sum of Squared Errors",xlab="Coefficient",xlim=c(2,4),ylim=c(10,18),xaxs="i",yaxs="i",pch=1,bty="n",lwd=wd)
axis(side=1,seq(2,4,.5),lwd=wd)
axis(side=2,lwd=wd)
points(beta_min,sse_min,pch=19,cex=.8)
text(beta_min,11,expression(hat(beta)))
text(2.5,16,expression(paste("SSE",(beta))))
dev.off() 

postscript("HANSEN3-1b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(beta_seq,sse_beta,type="l",lty=1,ylab="Sum of Squared Errors",xlab="Coefficient",xlim=c(2,4),ylim=c(10,18),xaxs="i",yaxs="i",pch=1,bty="n",lwd=wd)
axis(side=1,seq(2,4,.5),lwd=wd)
axis(side=2,lwd=wd)
points(beta_min,sse_min,pch=19,cex=.8)
text(beta_min,11,expression(hat(beta)))
text(2.5,16,expression(paste("SSE",(beta))))
dev.off() 
