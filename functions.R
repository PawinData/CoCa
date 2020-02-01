# Input: a vector of W_j, j=1,...,N
# Output: log(sum(exp(W_j)))

logspace_add <- function(w1,w2) 
{ 
  return(pmax(w1,w2) + log1p(exp(-abs(w1 - w2))))
}

logspace_add_mult <- function(w) 
{
  Reduce(logspace_add, w)
}

# inverse of CDF
# PDF: p(x)=|x|
# input: 0 <= y <= 1 (vector)
# output: x ~ p(x) (vector)
F_inv <- function(y)
{
  x <- rep(0,length(y))
  for (i in 1:length(y))
    {
      if (y[i] < 0.5)
        {
          x[i] <- - sqrt(1-2*y[i])
        }
      else
        {
          x[i] <- sqrt(2*y[i] - 1)
        }
  }
  return(x)
}