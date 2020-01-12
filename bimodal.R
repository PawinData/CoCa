m <- 3
n <- 100
source("L_causal.R")
source("L_confounded.R")

L_ca <- rep(0,20)
L_co <- rep(0,20)
L_CA <- rep(0,20)
L_CO <- rep(0,20)
for (t in 1:20)
{
  Z <- matrix(rbeta(m*n, 0.5, 0.5), nrow=n, ncol=m)
  H <- matrix(rnorm(m*(m+1), mean=0, sd=1), nrow=m, ncol=(m+1))
  X <- Z %*% H 
  Y <- X[,m+1]
  X <- X[,1:m]
  L_ca[t] <- L_causal(X,Y,500)
  L_co[t] <- L_confounded(X,Y,500)
  w <- matrix(rmvnorm(1, mean=rep(0,m), sigma=diag(m)), nrow=m, ncol=1)
  y <- X %*% w
  L_CA[t] <- L_causal(X,y,500)
  L_CO[t] <- L_confounded(X,y,500)
}
par(mfrow=c(1,2))
plot(1:20, L_ca, col="red", type="l", xlab="the ith run", ylab="MDL score",
     main=paste("Synthetic Bimodal Data \n (Confounded)"),
     sub="Beta(0.5,0.5)", ylim=c(0,1000))
lines(1:20, L_co, col="darkolivegreen")
legend("topleft",legend=c("L_ca","L_co"), 
       col=c("red","darkolivegreen"), pch=c("-","-"))
plot(1:20, L_CA, col="red", type="l", xlab="the ith run", ylab="MDL score",
     main=paste("Synthetic Bimodal Data \n (Causal)"),
     sub="Beta(0.5,0.5)", ylim=c(0,1000))
lines(1:20, L_CO, col="darkolivegreen")
legend("topleft",legend=c("L_ca","L_co"), 
       col=c("red","darkolivegreen"), pch=c("-","-"))