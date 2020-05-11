# Valley shaped functions


# 2 Dimensions -----------------------

#' Camel6
#' Six-hump camel function
#' @details 2-dimensional
Camel6 <- function(x)
{
  if(length(x) != 2) 
    stop("Wrong length of 'x'. Bohachevsky is defined for exactly 2 dimensions.")

  x1.2 <- x[1]^2
  s1 <- ( 4 - 2.1*x1.2 + (x[1]^4)/3 ) * x1.2
  s3 <- (4*x1.2 - 4) * x1.2
  
  return( s1 + x1.2 + s3 )
}


# N Dimensions -----------------------

#' RosbkExt
#' The extended Rosenbrock function (sometimes also Valley or Banana function), 
#' is a popular test problem for gradient-based optimization algorithms.
RosbkExt <- function(x) {
  n <- length(x)
  sum (100*(x[1:(n-1)]^2 - x[2:n])^2 + (x[1:(n-1)] - 1)^2)
}


#' DixonPrice
DixonPrice <- function(x) {
  d <- length(x)
  x1.2 <- (x[1]-1)^2
  v <- 2:d
  s <- sum( v * (2*x[v]^2 - x[1:(d-1)])^2 )
  
  return(x1.2 + s)
}
