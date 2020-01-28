# compute the MDL-score for observed (X,Y) under purely causal framework
# X: n-by-m matrix
# Y: n-by-1 vector
# N: sample size for approximating an integral with a sum
library(mvtnorm)
source("MESSY.R")

L_causal <- function(X, Y, N)
{
  n <- ncol(X)  # number of observations
  m <- nrow(X)  # dimension of X
  # compute log[P(X)]
  L_ca <- 0
  D <- diag(var(t(X))) * diag(m)
  for (i in 1:n)
  {
    L_ca <- L_ca + dmvnorm(X[,i], mean=rep(0,m), sigma=D, log=TRUE)
  }
  # compute logN - log[P(X)]
  L_ca <- log(N) - L_ca
  # compute log(sum(P(Y|X,w_q)))
  vector <- rep(0,N)
  for (q in 1:N)
  {
    w_sim <- rmvnorm(1, mean=rep(0,m), sigma=diag(m))
    Ep <- w_sim %*% X
    for (i in 1:n)
    {
      vector[q] <- vector[q] + dnorm(Y[i], mean=Ep[i], sd=sd(Y), log=TRUE)
    }
  }
  # compute L_ca
  L_ca <- L_ca - logspace_add_mult(vector)
  return(L_ca)
}