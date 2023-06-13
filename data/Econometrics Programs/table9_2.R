######################################
### This file generates Table 9.2. 
### Type I Error Probability of 
### Asymptotic 5% W(s) test
######################################

sink("Table9_2.log")

# set seed
set.seed(92)

# define wald statistic as a function of s and beta_hat
wald_s <- function(s,beta,sigma,n){
  val <- n*(beta^s-1)^2/(s^2)/(beta^(2*s-2))/(sigma^2)
  return(val)
}

# define simulation 
rej_freq <- Vectorize(function(s,n,sigma){
  y <- matrix(rnorm(n*50000),n)*sigma+1
  beta <- colMeans(y)
  w <- matrix(lapply(beta,function(beta) wald_s(s,beta,sigma,n)))
  prob  <- mean(w>3.84)
  return(prob)
})

s_vec <- seq(1,10)
n_vec <- c(20,100,500)
store1 <- outer(s_vec,n_vec,rej_freq,1)
store3 <- outer(s_vec,n_vec,rej_freq,3)
print(cbind(store1,store3))

sink()