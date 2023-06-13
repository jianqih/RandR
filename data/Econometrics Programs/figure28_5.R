#########################################
### This file executes the empirical work
### in Chapter 28
### including Figures 28.4a and 28.5ab
#########################################
### Uses packages haven, quadprog
### Uses data file cps09mar.dta
#########################################

library(haven)
library(quadprog)

cps <- read_dta("cps09mar.dta")
cps <- cps[cps$race==4,]
cps <- cps[cps$female==1,]

attach(cps)
 y <- matrix(log(earnings/hours/week))
 edu <- education
 ex <- age - edu - 6
 married <- (marital<=3)
 ne <- (region==1)
 mw <- (region==2)
 south <- (region==3)
 west <- (region==4)
detach(cps)

n <- length(y)
mx  <- max(ex)
exx <- ((ex/mx)%*%matrix(1,1,6))^(matrix(1,n,1)%*%t(matrix(1:6)))
college <- matrix(edu>=16)
edu9   <- (edu-9)*(edu<9)
edu12 <- (edu==12)
edu13 <- (edu==13)
edu14 <- (edu==14)
edu16 <- (edu==16)
edu18 <- (edu==18)
edu20 <- (edu==20)
ed <- cbind(edu12,edu13,edu14,edu16,edu18,edu20)

M <- 9
theta <- matrix(0,1,M)
v <- matrix(0,1,M)
kk <- matrix(0,1,M)
e <- matrix(0,n,M)
r <- matrix(0,n,M)
Y30 <- 30/mx
R <- Y30^as.matrix(1:6)

x1 <- as.matrix(cbind(exx[,1:2],college,married,ne,mw,south,matrix(1,n,1)))
k1 <- ncol(x1)
invx1 <-solve(t(x1)%*%x1)
b1 <- invx1%*%(t(x1)%*%y)
e1 <- y-x1%*%b1
leverage1 <- rowSums(x1*(x1%*%invx1))
r1 <- e1/(1-leverage1)
xe1 <- x1*rep(e1,times=k1)
V1 <- (n/(n-k1))*invx1%*%(t(xe1)%*%xe1)%*%invx1
theta[1] <- t(R[1:2])%*%b1[1:2]
v[1] <- t(R[1:2])%*%V1[1:2,1:2]%*%R[1:2]
kk[1] <- k1
e[,1] <- e1
r[,1] <- r1

x2 <- as.matrix(cbind(exx[,1:2],edu,edu9,married,ne,mw,south,matrix(1,n,1)))
k2 <- ncol(x2)
invx2 <-solve(t(x2)%*%x2)
b2 <- invx2%*%(t(x2)%*%y)
e2 <- y-x2%*%b2
leverage2 <- rowSums(x2*(x2%*%invx2))
r2 <- e2/(1-leverage2)
xe2 <- x2*rep(e2,times=k2)
V2 <- (n/(n-k2))*invx2%*%(t(xe2)%*%xe2)%*%invx2
theta[2] <- t(R[1:2])%*%b2[1:2]
v[2] <- t(R[1:2])%*%V2[1:2,1:2]%*%R[1:2]
kk[2] <- k2
e[,2] <- e2
r[,2] <- r2

x3 <- as.matrix(cbind(exx[,1:2],ed,married,ne,mw,south,matrix(1,n,1)))
k3 <- ncol(x3)
invx3 <-solve(t(x3)%*%x3)
b3 <- invx3%*%(t(x3)%*%y)
e3 <- y-x3%*%b3
leverage3 <- rowSums(x3*(x3%*%invx3))
r3 <- e3/(1-leverage3)
xe3 <- x3*rep(e3,times=k3)
V3 <- (n/(n-k3))*invx3%*%(t(xe3)%*%xe3)%*%invx3
theta[3] <- t(R[1:2])%*%b3[1:2]
v[3] <- t(R[1:2])%*%V3[1:2,1:2]%*%R[1:2]
kk[3] <- k3
e[,3] <- e3
r[,3] <- r3

x4 <- as.matrix(cbind(exx[,1:4],college,married,ne,mw,south,matrix(1,n,1)))
k4 <- ncol(x4)
invx4 <-solve(t(x4)%*%x4)
b4 <- invx4%*%(t(x4)%*%y)
e4 <- y-x4%*%b4
leverage4 <- rowSums(x4*(x4%*%invx4))
r4 <- e4/(1-leverage4)
xe4 <- x4*rep(e4,times=k4)
V4 <- (n/(n-k4))*invx4%*%(t(xe4)%*%xe4)%*%invx4
theta[4] <- t(R[1:4])%*%b4[1:4]
v[4] <- t(R[1:4])%*%V4[1:4,1:4]%*%R[1:4]
kk[4] <- k4
e[,4] <- e4
r[,4] <- r4

x5 <- as.matrix(cbind(exx[,1:4],edu,edu9,married,ne,mw,south,matrix(1,n,1)))
k5 <- ncol(x5)
invx5 <-solve(t(x5)%*%x5)
b5 <- invx5%*%(t(x5)%*%y)
e5 <- y-x5%*%b5
leverage5 <- rowSums(x5*(x5%*%invx5))
r5 <- e5/(1-leverage5)
xe5 <- x5*rep(e5,times=k5)
V5 <- (n/(n-k5))*invx5%*%(t(xe5)%*%xe5)%*%invx5
theta[5] <- t(R[1:4])%*%b5[1:4]
v[5] <- t(R[1:4])%*%V5[1:4,1:4]%*%R[1:4]
kk[5] <- k5
e[,5] <- e5
r[,5] <- r5

x6 <- as.matrix(cbind(exx[,1:4],ed,married,ne,mw,south,matrix(1,n,1)))
k6 <- ncol(x6)
invx6 <-solve(t(x6)%*%x6)
b6 <- invx6%*%(t(x6)%*%y)
e6 <- y-x6%*%b6
leverage6 <- rowSums(x6*(x6%*%invx6))
r6 <- e6/(1-leverage6)
xe6 <- x6*rep(e6,times=k6)
V6 <- (n/(n-k6))*invx6%*%(t(xe6)%*%xe6)%*%invx6
theta[6] <- t(R[1:4])%*%b6[1:4]
v[6] <- t(R[1:4])%*%V6[1:4,1:4]%*%R[1:4]
kk[6] <- k6
e[,6] <- e6
r[,6] <- r6

x7 <- as.matrix(cbind(exx,college,married,ne,mw,south,matrix(1,n,1)))
k7 <- ncol(x7)
invx7 <-solve(t(x7)%*%x7)
b7 <- invx7%*%(t(x7)%*%y)
e7 <- y-x7%*%b7
leverage7 <- rowSums(x7*(x7%*%invx7))
r7 <- e7/(1-leverage7)
xe7 <- x7*rep(e7,times=k7)
V7 <- (n/(n-k7))*invx7%*%(t(xe7)%*%xe7)%*%invx7
theta[7] <- t(R)%*%b7[1:6]
v[7] <- t(R)%*%V7[1:6,1:6]%*%R
kk[7] <- k7
e[,7] <- e7
r[,7] <- r7

x8 <- as.matrix(cbind(exx,edu,edu9,married,ne,mw,south,matrix(1,n,1)))
k8 <- ncol(x8)
invx8 <-solve(t(x8)%*%x8)
b8 <- invx8%*%(t(x8)%*%y)
e8 <- y-x8%*%b8
leverage8 <- rowSums(x8*(x8%*%invx8))
r8 <- e8/(1-leverage8)
xe8 <- x8*rep(e8,times=k8)
V8 <- (n/(n-k8))*invx8%*%(t(xe8)%*%xe8)%*%invx8
theta[8] <- t(R)%*%b8[1:6]
v[8] <- t(R)%*%V8[1:6,1:6]%*%R
kk[8] <- k8
e[,8] <- e8
r[,8] <- r8

x9 <- as.matrix(cbind(exx,ed,married,ne,mw,south,matrix(1,n,1)))
k9 <- ncol(x9)
invx9 <-solve(t(x9)%*%x9)
b9 <- invx9%*%(t(x9)%*%y)
e9 <- y-x9%*%b9
leverage9 <- rowSums(x9*(x9%*%invx9))
r9 <- e9/(1-leverage9)
xe9 <- x9*rep(e9,times=k9)
V9 <- (n/(n-k9))*invx9%*%(t(xe9)%*%xe9)%*%invx9
theta[9] <- t(R)%*%b9[1:6]
v[9] <- t(R)%*%V9[1:6,1:6]%*%R
kk[9] <- k9
e[,9] <- e9
r[,9] <- r9

sig <- colMeans(e^2)
bic <- n*log(2*pi*sig) + kk*log(n)
aic <- n*log(2*pi*sig) + kk*2
cv <- colSums(r^2)
fic <- n*(theta-theta[9])^2 + 2*n*v
se <- sqrt(v)

# Model Selection

cat("Return to Experience\n")
print(theta)
cat("Standard Error\n")
print(se)
cat("BIC\n")
print(bic)
cat("AIC\n")
print(aic)
cat("CV\n")
print(cv)
cat("FIC\n")
print(fic)

# Shrinkage for experience profile

x <- (0:50)
xn <- length(x)
xm <- x/mx
c3 <- colMeans(x3[,3:k3])%*%b3[3:k3] 
f3 <- cbind(xm,xm^2)%*%b3[1:2] + matrix(c3,xn,1)
c9 <- colMeans(x9[,7:k9])%*%b9[7:k9] 
f9 <- cbind(xm,xm^2,xm^3,xm^4,xm^5,xm^6)%*%b9[1:6] + matrix(c9,xn,1)

S <- rbind(matrix(0,2,4),diag(4),matrix(0,11,4))
D <- (V9%*%S)%*%solve(t(S)%*%V9%*%S)%*%(t(S)%*%b9)
J <- (k9-k3)/(t(D)%*%solve(V9)%*%D)
if (J > 1) {J=1}
bJS <- b9 - D%*%J

cJS <- colMeans(x9[,7:17])%*%bJS[7:17] 
fJS <- cbind(xm,xm^2,xm^3,xm^4,xm^5,xm^6)%*%bJS[1:6] + matrix(cJS,xn,1)

cat("Shrinkage for Return to Experience\n")
cat("Shrinkage weight\n")
print(J)

wd <- 1.4

pdf("HANSEN28-4a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,f3,type="l",lty=2,xaxs="i",yaxs="i",ylab="Log Wage",xlim=c(0,50),ylim=c(2.5,3.1),xlab="Experience (Years)",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,lwd=wd)
lines(x,f9,lty=5,lwd=wd)
lines(x,fJS,lty=1,lwd=wd)
legend("bottomright",legend=c("Model 3","Model 9","James-Stein"),lty=c(2,5,1),lwd=wd,bty="n")
dev.off()

postscript("HANSEN28-4a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,f3,type="l",lty=2,xaxs="i",yaxs="i",ylab="Log Wage",xlim=c(0,50),ylim=c(2.5,3.1),xlab="Experience (Years)",xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(0,50,10),lwd=wd)
axis(side=2,lwd=wd)
lines(x,f9,lty=5,lwd=wd)
lines(x,fJS,lty=1,lwd=wd)
legend("bottomright",legend=c("Model 3","Model 9","James-Stein"),lty=c(2,5,1),lwd=wd,bty="n")
dev.off()


# Shrinkage Across Regions

cps <- read_dta("cps09mar.dta")
cps <- cps[cps$race==2,]
cps <- cps[cps$female==0,]

attach(cps)
 y <- matrix(log(earnings/hours/week))
 edu <- education
 ex <- age - edu - 6
 married <- (marital<=3)
 ne <- (region==1)
 mw <- (region==2)
 south <- (region==3)
 west <- (region==4)
detach(cps)

n <- length(y)
mx  <- max(ex)
exx <- ((ex/mx)%*%matrix(1,1,6))^(matrix(1,n,1)%*%t(matrix(1:6)))
college <- matrix(edu>=16)
edu9   <- (edu-9)*(edu<9)
edu12 <- (edu==12)
edu13 <- (edu==13)
edu14 <- (edu==14)
edu16 <- (edu==16)
edu18 <- (edu==18)
edu20 <- (edu==20)
ed <- cbind(edu12,edu13,edu14,edu16,edu18,edu20)

nd <- ncol(ed)
reg <- cbind(ne,mw,south,west)
x10 <- as.matrix(cbind(ed*rep(ne,times=nd),ed*rep(mw,times=nd),ed*rep(south,times=nd),ed*rep(west,times=nd),reg,married,exx[,1:4]))
k10 <- ncol(x10)
invx10 <-solve(t(x10)%*%x10)
b10 <- invx10%*%(t(x10)%*%y)
e10 <- y-x10%*%b10
V10 <- invx10*(sum(e10^2)/(n-k10))

x <- as.matrix(c(11,12,13,14,16,18,20))
nx <- length(x) - 1
c <- colMeans(x10[,29:k10])%*%b10[29:k10] 
fne <- rbind(0,matrix(b10[1:nx])) + matrix(c+b10[25],nx+1,1)
fmw <- rbind(0,matrix(b10[(nx+1):(2*nx)])) + matrix(c+b10[26],nx+1,1)
fsouth <- rbind(0,matrix(b10[(2*nx+1):(3*nx)])) + matrix(c+b10[27],nx+1,1)
fwest <- rbind(0,matrix(b10[(3*nx+1):(4*nx)])) + matrix(c+b10[28],nx+1,1)

S <- matrix(0,k10,nd*3)
S[1:(2*nd),1:nd] <- rbind(diag(nd),-diag(nd))
S[(nd+1):(3*nd),(nd+1):(2*nd)] <- rbind(diag(nd),-diag(nd))
S[(2*nd+1):(4*nd),(2*nd+1):(3*nd)] <- rbind(diag(nd),-diag(nd))
D <- (V10%*%S)%*%solve(t(S)%*%V10%*%S)%*%(t(S)%*%b10)
J <- (nd*3-2)/(t(D)%*%solve(V10)%*%D)
if (J > 1) {J=1}
bJSed <- b10 - D%*%J

c <- colMeans(x10[,29:k10])%*%bJSed[29:k10] 
fneJS <- rbind(0,matrix(bJSed[1:nx])) + matrix(c+bJSed[25],nx+1,1)
fmwJS <- rbind(0,matrix(bJSed[(nx+1):(2*nx)])) + matrix(c+bJSed[26],nx+1,1)
fsouthJS <- rbind(0,matrix(bJSed[(2*nx+1):(3*nx)])) + matrix(c+bJSed[27],nx+1,1)
fwestJS <- rbind(0,matrix(bJSed[(3*nx+1):(4*nx)])) + matrix(c+bJSed[27],nx+1,1)

cat("Shrinkage Across Regions\n")
cat("Shrinkage weight\n")
print(J)

pdf("HANSEN28-5a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,fne,type="l",lty=2,xaxs="i",yaxs="i",ylab="Log Wage",ylim=c(2,4),xlab="Education (Years)",bty="n",xaxt="n",lwd=wd)
axis(side=1,seq(10,20,2),lwd=wd)
axis(side=2,lwd=wd)
lines(x,fmw,lty=5,lwd=wd)
lines(x,fsouth,lty=1,lwd=wd)
lines(x,fwest,lty=6,lwd=wd)
legend("bottomright",legend=c("Northeast","Midwest","South","West"),lty=c(2,5,1,6),lwd=wd,bty="n")
dev.off()

postscript("HANSEN28-5a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,fne,type="l",lty=2,xaxs="i",yaxs="i",ylab="Log Wage",ylim=c(2,4),xlab="Education (Years)",bty="n",xaxt="n",lwd=wd)
axis(side=1,seq(10,20,2),lwd=wd)
axis(side=2,lwd=wd)
lines(x,fmw,lty=5,lwd=wd)
lines(x,fsouth,lty=1,lwd=wd)
lines(x,fwest,lty=6,lwd=wd)
legend("bottomright",legend=c("Northeast","Midwest","South","West"),lty=c(2,5,1,6),lwd=wd,bty="n")
dev.off()


pdf("HANSEN28-5b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(x,fneJS,type="l",lty=2,xaxs="i",yaxs="i",ylab="Log Wage",ylim=c(2,4),xlab="Education (Years)",bty="n",xaxt="n",lwd=wd)
axis(side=1,seq(10,20,2),lwd=wd)
axis(side=2,lwd=wd)
lines(x,fmwJS,lty=5,lwd=wd)
lines(x,fsouthJS,lty=1,lwd=wd)
lines(x,fwestJS,lty=6,lwd=wd)
legend("bottomright",legend=c("Northeast","Midwest","South","West"),lty=c(2,5,1,6),lwd=wd,bty="n")
dev.off()

postscript("HANSEN28-5b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(x,fneJS,type="l",lty=2,xaxs="i",yaxs="i",ylab="Log Wage",ylim=c(2,4),xlab="Education (Years)",bty="n",xaxt="n",lwd=wd)
axis(side=1,seq(10,20,2),lwd=wd)
axis(side=2,lwd=wd)
lines(x,fmwJS,lty=5,lwd=wd)
lines(x,fsouthJS,lty=1,lwd=wd)
lines(x,fwestJS,lty=6,lwd=wd)
legend("bottomright",legend=c("Northeast","Midwest","South","West"),lty=c(2,5,1,6),lwd=wd,bty="n")
dev.off()


# Model Averaging

dbic <- bic-min(bic)
daic <- aic-min(aic)
wbic <- exp(-dbic/2)
wbic <- wbic/sum(wbic)
waic <- exp(-daic/2)
waic <- waic/sum(waic)

Dmat <- t(e)%*%e 
dmat <- -kk*(sum(e[,9]^2)/(n-kk[9]))
Amat <- t(rbind(matrix(1,nrow=1,ncol=M),diag(M),-diag(M)))
bvec <- rbind(1,matrix(0,nrow=M,ncol=1),matrix(-1,nrow=M,ncol=1))
QP <- solve.QP(Dmat,dmat,Amat,bvec,1)
wmallows <- QP$solution

Dmatr <- t(r)%*%r 
Amat <- t(rbind(matrix(1,nrow=1,ncol=M),diag(M),-diag(M)))
bvec <- rbind(1,matrix(0,nrow=M,ncol=1),matrix(-1,nrow=M,ncol=1))
QP2 <- solve.QP(Dmatr,matrix(0,1,M),Amat,bvec,1)
wJMA <- QP2$solution

thetawbic <- sum(theta*wbic)
thetawaic <- sum(theta*waic)
thetaMMA <- sum(theta*wmallows)
thetaJMA <- sum(theta*wJMA)


cat("DBIC\n")
print(dbic)
cat("BIC Weights\n")
print(wbic)
cat("DAIC\n")
print(daic)
cat("AIC weights\n")
print(waic)
cat("Mallows weights\n")
print(wmallows)
cat("JMA weights\n")
print(wJMA)
cat("Estimates\n")
print(cbind(thetawbic,thetawaic,thetaMMA,thetaJMA))


