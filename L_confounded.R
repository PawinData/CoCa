# compute the MDL-score for observed (X,Y) under purely causal framework
# X: n-by-m matrix
# Y: n-by-1 vector
# N: sample size for approximating an integral with a sum
library(mvtnorm)
source("MESSY.R")

L_confounded <- function(X,Y,N)
{
  n <- ncol(X) # number of observations
  m <- nrow(X)  # dimension of X
  X_Y <- rbind(X,Y)
  # compute the joint conditional probability of each observation
  # take log
  VECTOR <- rep(0,N)
  v <- matrix(rep(0,m), nrow=1, ncol=m)
  SIGMA <- rbind(cbind(diag(var(t(X)))*diag(m),t(v)), cbind(v,var(t(Y))*diag(1)))
  D <- diag(m)
  for (q in 1:N)
  {
    Z_sim <- t(rmvnorm(1, mean=v, sigma=D))
    H_sim <- matrix(rnorm(m*(m+1),mean=0,sd=1), nrow=m+1, ncol=m)
    Ep <- H_sim %*% Z_sim
    for (i in 1:n)
    {
      VECTOR[q] <- VECTOR[q] + dmvnorm(t(X_Y[,i]), mean=Ep, sigma=SIGMA, log = TRUE)
    }
  }
  # compute_L_co
  L_co <- log(N) - logspace_add_mult(VECTOR)
  return(L_co)
}