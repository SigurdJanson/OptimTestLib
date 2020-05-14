# Convex functions



# 2 Dimensions -----------------------


Ackley2 <- function(x) {
  return( -200 * exp(0.02*sqrt(x[1]^2 + x[2]^2)) )
}


#' Bohachevsky
#' Computes first Bohachevsky function at point `x`.
#' @param x Vector with two positions
#' @details 
#' Properties of this function:
#' * continuous
#' * unimodal
#' * convex
#' * defined on 2-dimensional space
#' @return A scalar
#' @export
#' @examples Bohachevsky(c(1, 1))
Bohachevsky <- function( x ) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. Bohachevsky is defined for exactly 2 dimensions.")

  s3 <- -0.3 * cos(3*pi*x[1])
  s4 <- -0.4 * cos(4*pi*x[2])
  z <- x[1]^2 + 2*x[2]^2 + s3 + s4 + 0.7
  
  return(z)
}



# N Dimensions -----------------------

#' Sphere
#' Computes the value of the Sphere function.
#' @param x  Numeric vector
#' @details 
#' Properties of this function:
#' * continuous
#' * unimodal
#' * convex
#' * defined on n-dimensional space
#' * differentiable
#' * separable
#' @return A scalar
#' @export
#'
#' @examples
Sphere <- function(x) {sum(x^2)}



#' SumSquare
#' Computes the value of sum of squares function.
#' @param x  Numeric vector
#' @details 
#' Usually evaluated on $x_i ∈ [−10, 10] for i=1,… , n$.
#' Properties of this function:
#' * continuous
#' * unimodal
#' * convex
#' * defined on n-dimensional space
#' * differentiable
#' * separable
#' @return A scalar
#' @export
#'
#' @examples
SumSquare <- function(x) {
  sum(1:length(x) * x^2)
}


#' Schwefel223
#' 
#' @param x  Numeric vector
#' @details 
#' Usually evaluated on $x_i ∈ [−10, 10] for i=1,… , n$.
#' Properties of this function:
#' * Continuous
#' * Differentiable
#' * Non-Separable
#' * Scalable
#' * Unimodal
#' @return A scalar
#' @export
Schwefel223 <- function(x) {
  sum(x^10)
}