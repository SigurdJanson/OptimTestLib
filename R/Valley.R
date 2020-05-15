# Valley shaped functions


# 2 Dimensions -----------------------

#' Camel6
#' Six-hump camel function
#' @param x Numeric vector or length 2 representing a coordinate in a parameter space
#' @return A scalar
#' @source Function 30 in Jamil, M., & Yang, X. S. (2013). A literature survey of benchmark functions for global optimisation problems. International Journal of Mathematical Modelling and Numerical Optimisation, 4(2), 150. https://doi.org/10.1504/ijmmno.2013.055204
#' @export
Camel6 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. Camel6 is defined for exactly 2 dimensions.")

  x1.2 <- x[1]^2
  s1 <- ( 4 - 2.1*x1.2 + (x[1]^4)/3 ) * x1.2
  s3 <- (4*x1.2 - 4) * x1.2
  
  return( s1 + x1.2 + s3 )
}



# N Dimensions -----------------------

#' RosbkExt
#' The extended Rosenbrock function (sometimes also Valley or Banana function), 
#' is a popular test problem for gradient-based optimization algorithms.
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' @source 
#' @export
RosbkExt <- function(x) {
  n <- length(x)
  sum (100*(x[1:(n-1)]^2 - x[2:n])^2 + (x[1:(n-1)] - 1)^2)
}


#' DixonPrice
#' 
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' @source https://www.sfu.ca/~ssurjano/dixonpr.html
#' @export
DixonPrice <- function(x) {
  d <- length(x)
  x1.2 <- (x[1]-1)^2
  v <- 2:d
  s <- sum( v * (2*x[v]^2 - x[1:(d-1)])^2 )
  
  return(x1.2 + s)
}



#' StybTang
#' Styblinski-Tang function
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' @source Function 144 in Jamil, M., & Yang, X. S. (2013). A literature survey of benchmark functions for global optimisation problems. International Journal of Mathematical Modelling and Numerical Optimisation, 4(2), 150. https://doi.org/10.1504/ijmmno.2013.055204
#' @export
StybTang <- function(x) {
  return(sum(x^4 - 16*x^2 + 5*x) / 2)
}