######################################
### This file generates Table 9.3. 
### Type I Error Probability of 
### Asymptotic 5% t-test
######################################

sink("Table9_3.log")

# set seed
set.seed(93)

# define function to obtain statistics  
rej_freq <- function(beta,sig,n){
  r <- 1/beta[3]
  x <- matrix(rnorm(n*2),n)
  x1 <- cbind(matrix(1,n,1),x)
  y <- x1%*%beta + rnorm(n)*sig
  lm.obj <- lm(y~x)
  betahat <- lm.obj$coefficients
  #e <- y-x1%*%betahat
  #xe <- t(x1)%*%e
  #xxi <- solve(t(x1)%*%x1)
  #v <- xxi %*% xe %*% t(xe) %*% xxi
  v <- vcov(lm.obj)
  rhat <- betahat[2]/betahat[3]
  h1 <- c(0,1/betahat[2],-rhat/betahat[3])
  h2 <- c(0,1,-r)
  t1 <- (rhat-r)/sqrt(t(h1)%*%v%*%h1)
  t2 <- (betahat[2]-betahat[3]*r)/sqrt(t(h2)%*%v%*%h2)
  stat <- c(t1,t2)
  return(stat)
}

# define function for simulation
rep_simul <- function(beta2,n){
  rep <- 50000
  beta <- c(0,1,beta2)
  sig <- 3
  stat <- replicate(rep,rej_freq(beta,sig,n))
  prob <- rowMeans(rbind(stat< -1.645,stat>1.645))
  return(prob)
}

beta2_vec <- c(.1,.25,.50,.75,1)
store100 <- matrix(unlist(lapply(beta2_vec,rep_simul,100)),4)
store500 <- matrix(unlist(lapply(beta2_vec,rep_simul,500)),4)

print(cbind(t(store100),t(store500)))

sink()