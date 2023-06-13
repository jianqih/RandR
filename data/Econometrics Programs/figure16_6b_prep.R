#####################################
### This file generates the prep work for Figure 16.6b
### Spurious Regression T Coverage
#####################################

set.seed(719)
rep <- 1000000
ns <- seq(10,200,10)
nn <- length(ns)
coverage <- matrix(0,nn,2)
coverage[,1] <- ns

for (ni in 1:nn){
n <- ns[ni]

c <- qf(.95,1,n-2)
store <- matrix(0,rep,1)
for (i in 1:rep){
  y <- as.matrix(cumsum(rnorm(n)))
  x <- as.matrix(cumsum(rnorm(n)))
  y <- y - mean(y)
  x <- x - mean(x)
  mx <- sum(x^2)
  beta <- sum(x*y)/mx
  v <- mean((y-x%*%beta)^2)/mx
  store[i] <- ((beta^2)/v <= c)
}
coverage[ni,2] <- mean(store)
}
write.table(coverage,file="figure16_6b.txt",sep="\t",row.names=FALSE,col.names=FALSE)
