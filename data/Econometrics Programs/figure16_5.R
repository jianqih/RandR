#####################################
### This file generates Figure 16.5
### Spurious Regression
#####################################
### Uses package haven
### Uses dataset FRED-QD.dta
#########################################

library(haven)

n <- 476
set.seed(1672020)
y1 <- cumsum(rnorm(n))
y2 <- cumsum(rnorm(n))
x <- seq(1980,2020,length=n)
y1m <- (y1-mean(y1))/sd(y1)
y2m <- (y2-mean(y2))/sd(y2)

beta <- sum(y1m*y2m)/sum(y2m^2)
e <- y1m - y2m*beta
se1 <- sqrt(mean(e^2)/sum(y2m^2))
se2 <- sqrt(sum((y2m*e)^2)/(sum(y2m^2))^2)
R2 <- 1 - var(e)/var(y1m)

X <- cbind(matrix(1,n-1,1),y2[2:n],y1[1:(n-1)])
Y <- y1[2:n]
XX <- solve(crossprod(X))
bbeta <- XX%*%crossprod(X,Y)
s <- sqrt(diag(XX*mean((Y - X%*%bbeta)^2)))

cat("Random Walks \n")
cat("Correlation \n")
print(cor(y1,y2))
cat("Beta, se1, se2 \n")
print(cbind(beta,se1,se2))
cat("T\n")
print(beta/se1)
cat("R2 \n")
print(R2)
cat("Dynamic Regression \n")
print(cbind(bbeta,s))
cat("\n")

wd <- 1.4

pdf("HANSEN16-5a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,y1m,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",yaxt="n",xaxt="n",lty=2,lwd=wd) 
axis(side=1,seq(1975,2025,5),lwd=wd)
lines(x,y2m,lty=1,lwd=wd)
legend("topleft",c("Series 1", "Series 2"), lty=c(2,1),lwd=wd,bty="n")
dev.off()

postscript("HANSEN16-5a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,y1m,type="l",xaxs="i",yaxs="i",ylab="",xlab="",bty="n",yaxt="n",xaxt="n",lty=2,lwd=wd) 
axis(side=1,seq(1975,2025,5),lwd=wd)
lines(x,y2m,lty=1,lwd=wd)
legend("topleft",c("Series 1", "Series 2"), lty=c(2,1),lwd=wd,bty="n")
dev.off()

data <- read_dta("FRED-QD.dta")
dat <- cbind(data$civpart,data$excausx)
datt <- ts(dat,frequency=4,start=c(1959,1))
y1 <- datt[,1]
y2 <- log(datt[,2])
y1m <- (y1-mean(y1))/sd(y1)
y2m <- (y2-mean(y2))/sd(y2)
y <- cbind(y1m,y2m)

pdf("HANSEN16-5b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(y,plot.type="single",xlab="",ylab="",bty="n",yaxt="n",xaxt="n",lty=c(2,1),lwd=wd)
axis(side=1,seq(1950,2030,10),lwd=wd)
legend("topleft",c("Participation Rate", "Exchange Rate"),lty=c(2,1),lwd=wd,bty="n")
dev.off()

postscript("HANSEN16-5b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(y,plot.type="single",xlab="",ylab="",bty="n",yaxt="n",xaxt="n",lty=c(2,1),lwd=wd)
axis(side=1,seq(1950,2030,10),lwd=wd)
legend("topleft",c("Participation Rate", "Exchange Rate"),lty=c(2,1),lwd=wd,bty="n")
dev.off()

beta <- sum(y1m*y2m)/sum(y2m^2)
e <- y1m - y2m*beta
se1 <- sqrt(mean(e^2)/sum(y2m^2))
se2 <- sqrt(sum((y2m*e)^2)/(sum(y2m^2))^2)
R2 <- 1 - var(e)/var(y1m)

n <- length(y1)
tr <- as.matrix(seq(1:(n-1)))
X <- cbind(matrix(1,n-1,1),tr,y2[2:n],y1[1:(n-1)])
Y <- y1[2:n]
XX <- solve(crossprod(X))
bbeta <- XX%*%crossprod(X,Y)
s <- sqrt(diag(XX*mean((Y - X%*%bbeta)^2)))

cat("Labor Force Participation and Exchange Rate \n")
cat("Correlation \n")
print(cor(y1,y2))
cat("Beta, se1, se2 \n")
print(cbind(beta,se1,se2))
cat("T\n")
print(beta/se1)
cat("R2 \n")
print(R2)
cat("Dynamic Regression \n")
print(cbind(bbeta,s))
