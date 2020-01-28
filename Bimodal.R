library(mvtnorm)
# introduce the functions
source("L_causal.R")
source("L_confounded.R")

# dimension of X and Z (assume equal)
m <- 6
# sample size of observations
n <- 100
# sample size of approximating integrals
N <- 500
# number of runs
RUN <- 20
TIME <- 1:RUN
par(mfrow=c(1,2))

# Simulate (X,Y) under Purely Causal Framework
L_ca <- rep(0,RUN)
L_co <- rep(0,RUN)
for (T in TIME)
{
  # Generate X from p(x)=|x|, -1<=x<=1, extremely deviated from N(0,1)
  X <- matrix(F_inv(runif(m*n)), nrow=m, ncol=n)
  w <- rmvnorm(1, mean=rep(0,m), sigma=diag(m))
  Y <- matrix(w %*% X, nrow=1, ncol=n) + matrix(rnorm(n, mean=0, sd=1), nrow=1, ncol=n) 
  # compute L_ca
  L_ca[T] <- L_causal(X,Y,N)
  # compute L_co
  L_co[T] <- L_confounded(X,Y,N)
}
plot(TIME, L_ca, type="l", col="red", xlab="The ith run", ylab="MDL-score",
     ylim=c(0.8 * min(L_ca, L_co), 1.2 * max(L_ca, L_co)),
     main=paste("Synthetic Data Generated under \n Purely Causal Framework"))
lines(TIME, L_co, col="darkolivegreen")
legend("topright",legend=c("L_ca","L_co"), pch=rep("-",2),
       col=c("red","darkolivegreen"))


# Simulate (X,Y) under Purely Confounded Framework
L_ca <- rep(0,RUN)
L_co <- rep(0,RUN)
for (T in TIME)
{
  # Generate Z from p(z)=|z|, -1<=z<=1, extremely deviated from N(0,1)
  Z <- matrix(F_inv(runif(m*n)), nrow=m, ncol=n)
  H <- matrix(rnorm(m*(m+1), mean=0, sd=1), nrow=m+1, ncol=m)
  X <- H %*% Z + matrix(rnorm((m+1)*n,mean=0,sd=1), nrow=m+1, ncol=n)
  Y <- matrix(X[m+1, ], nrow=1, ncol=n)
  X <- X[1:m, ]
  # compute L_ca
  L_ca[T] <- L_causal(X,Y,N)
  # compute L_co
  L_co[T] <- L_confounded(X,Y,N)
}
plot(TIME, L_ca, type="l", col="red", xlab="The ith run", ylab="MDL-score",
     ylim=c(0.8 * min(L_ca, L_co), 1.2 * max(L_ca, L_co)),
     main=paste("Synthetic Data Generated under \n Purely Confounded Framework"))
lines(TIME, L_co, col="darkolivegreen")
legend("topright",legend=c("L_ca","L_co"), pch=rep("-",2),
       col=c("red","darkolivegreen"))
