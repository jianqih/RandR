#####################################
### This file generates Figure 28.3
### MSE of Positive Part James-Stein Estimator
#####################################


x <- as.matrix(seq(0,5,by=0.01))
G <- length(x)

K <- c(4,6,12,48)
n <- length(K)
a <- as.matrix(1:500)

mse <- matrix(0,G,n)
for (ki in 1:n){    
  k <- K[ki]
  xk <- x*k
  F <- pchisq(k-2,k-2+a*2)
  for (gi in 1:G){
    lambda <- xk[gi]
    lambda2a <- cumprod((lambda/2)/a)
    EQ <- (1/(k-2) + sum(lambda2a/(k-2+a*2)))*exp(-lambda/2)
    JK <- (pchisq(k-2,k-2)/(k-2) + sum(F*lambda2a/(k-2+a*2)))*exp(-lambda/2)
    mse[gi,ki] <- 1 - EQ*(((k-2)^2)/k) - 2*pchisq(k-2,k,lambda) + pchisq(k-2,k+2,lambda) + lambda/k*pchisq(k-2,k+4,lambda) + ((k-2)^2)/k*JK
  }
}

pdf("HANSEN28-3.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,mse[,1],type="l",lty=1,xaxs="i",yaxs="i",ylim=c(0,1),ylab="MSE/K",xlab=expression(lambda/K),bty="n",cex.lab=.75,cex.axis=.75)
lines(x,mse[,2],lty=2)
lines(x,mse[,3],lty=6)
lines(x,mse[,4],lty=5)
legend("right",legend=c(expression(K==4),expression(K==6),expression(K==12),expression(K==48)),lty=c(1,2,6,5),cex=.75,bty="n")
dev.off()

postscript("HANSEN28-3.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,mse[,1],type="l",lty=1,xaxs="i",yaxs="i",ylim=c(0,1),ylab="MSE/K",xlab=expression(lambda/K),bty="n",cex.lab=.75,cex.axis=.75)
lines(x,mse[,2],lty=2)
lines(x,mse[,3],lty=6)
lines(x,mse[,4],lty=5)
legend("right",legend=c(expression(K==4),expression(K==6),expression(K==12),expression(K==48)),lty=c(1,2,6,5),cex=.75,bty="n")
dev.off()
