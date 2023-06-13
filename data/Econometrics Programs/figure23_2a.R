#####################################
### This file generates 
### Figure 23.2a Box-Cox NLLS Criterion
### Box-Cox NLLS estimation
#####################################
### This file uses the package haven
### This file uses the data file AJR2001.dta
#####################################

library(haven)

BoxCox <- function(x,lambda) if (lambda==0) y <- log(x) else y <- ((x^lambda)-1)/lambda 
DBoxCox <- function(x,lambda) if (lambda==0) D <- (log(x))^2 else D <- (1+(x^lambda)*(log(x)*lambda-1))/(lambda^2)

dat <- read_dta("AJR2001.dta")
x <- exp(dat$logmort0)
y <- dat$risk
n <- length(y)

SSE <- function(lambda,y) {
  Z <- BoxCox(x,lambda)
  X <- cbind(matrix(1,n,1),Z)
  b <- solve(crossprod(X,X),crossprod(X,y))
  e <- y - X%*%b
  sse <- mean(e^2)
  return(sse)
}

w <- seq(-2,1,.01)
wn <- length(w)
f <- matrix(0,wn,1)

for (j in 1:wn){
  theta <- w[j]
  f[j] <- SSE(w[j],y)
}

fmin <- min(f)
lambda <- w[which.min(f)]

wd <- 1.4

pdf("HANSEN23-2a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(w,f,type="l",lty=1,xaxs="i",yaxs="i",ylab="Average Squared Error Function",xlab="Box-Cox Coefficient",ylim=c(1.1,2),yaxt="n",lwd=wd,bty="n")
points(lambda,fmin,pch=19,cex=.8)
axis(side=1,lwd=wd)
axis(side=2,seq(1,2,.2),lwd=wd)
text(lambda,1.2,expression(hat(lambda)),cex=1.1)
text(.4,1.6,expression(S[n](lambda)))
dev.off()

postscript("HANSEN23-2a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(w,f,type="l",lty=1,xaxs="i",yaxs="i",ylab="Average Squared Error Function",xlab="Box-Cox Coefficient",ylim=c(1.1,2),yaxt="n",lwd=wd,bty="n")
points(lambda,fmin,pch=19,cex=.8)
axis(side=1,lwd=wd)
axis(side=2,seq(1,2,.2),lwd=wd)
text(lambda,1.2,expression(hat(lambda)),cex=1.1)
text(.4,1.6,expression(S[n](lambda)))
dev.off()

BC <- optimize(SSE,c(-1,1),y=y)
S1 <- SSE(1,y)
S0 <- SSE(0,y)
SM <- BC$objective
lambda <- BC$minimum
Z <- BoxCox(x,lambda)
D <- DBoxCox(x,lambda)
X <- cbind(matrix(1,n,1),Z)
b <- solve(crossprod(X,X),crossprod(X,y))
e <- y - X%*%b
X <- cbind(X,D*b[2])
Q <- solve(crossprod(X))
u <- e%*%matrix(1,1,3)
V <- Q%*%crossprod(X*u)%*%Q
s <- sqrt(diag(V))
beta <- rbind(b,lambda)
F <- n*(var(y)-SM)/SM

cat("Sigma-hat-squared, lambda = 1, 0, esimated \n")
print(cbind(S1,S0,SM))
cat("Coefficients \n")
print(cbind(beta,s))

# Multiplier Bootstrap Test
B <- 1000
Fstore <-matrix(0,B,1)
for (i in 1:B) {
  yi <- e*((runif(n)>0.5)*2-1)
  BCi <- optimize(SSE,c(-1,1),y=yi)
  SMi <- BCi$objective
  Fstore[i] <- n*(var(yi)-SMi)/SMi
}
pvalue <- mean(Fstore > F)

cat("F test for Significance: B, F, p-value \n")
print(cbind(B,F,pvalue))
