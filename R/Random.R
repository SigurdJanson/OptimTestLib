# Random
# Test functions with a random component



Quartic <- function(x) {
  d <- 1:length(x)
  e <- runif(1, max = 1-sqrt(.Machine$double.eps))
  return( sum(d * x^4) + e )
}



XinShe <- function(x) {
  d <- length(x)
  return( sum(runif(d) * abs(x)^d) )
}


