# Haystack functions having many local minima

# 2 Dimensions -----------------------


Keane <- function(x) {
  n <- sin(x[1]-x[2])^2 * sin(x[1]+x[2])^2
  return( n / sqrt(x[1]^2+x[2]^2) )
}


#' HolderTable1
#'
#' @param x  Numeric vector of length 2
#' @return A scalar
HolderTable1 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. HolderTable1 is defined for exactly 2 dimensions.")
  
  f1 <- cos(x[1])*cos(x[2])
  f2 <- exp( abs(1 - sqrt(x[1]^2 + x[2]^2) / pi) )
  
  return(-abs(f1*f2))
}


#' HolderTable2
#'
#' @param x  Numeric vector of length 2
#' @return A scalar
HolderTable2 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. HolderTable2 is defined for exactly 2 dimensions.")
  
  f1 <- sin(x[1])*cos(x[2])
  f2 <- exp( abs(1 - sqrt(x[1]^2 + x[2]^2) / pi) )
  
  return(-abs(f1*f2))
}


# ShafferF6
# TODO: formula unclear
# https://www.cs.unm.edu/~neal.holts/dga/benchmarkFunction/schafferf6.html
# Function 136
ShafferF6 <- function(x) {
  xsys <- x[1]^2 + x[2]^2
  return( 0.5 + (sin(sqrt(xsys))-0.5) / (1 + 0.001*xsys)^2 )
}


# N Dimensions -----------------------

#' Rastrigin
#'
#' @param x  Numeric vector
#' @return A scalar
Rastrigin <- function(x) {
  10*length(x) + sum((x)^2 - 10*cos(1*pi*(x)))
}


#' Schwefel
#' 
#' @param x Numeric vector
#' @return A scalar
#' ---TODO: not sure about this anymore - conflicting sources do not help
Schwefel <- function(x) {
  418.9829*length(x) - sum( x*sin(sqrt(abs(x))) )
}


#' Ackley
#' 
#' @param x Numeric vector
#' @param a = default value 20
#' @param b =  default value 0.2
#' @param c =  default value 2*pi
#' @details 
#' Properties of this function:
#' * Continuous
#' * Differentiable
#' * Non-separable
#' * Scalable
#' * Multimodal
#' @return A scalar
#' @references 
#' Bäck, T. & Schwefel, H. P. (1993). “An Overview of Evolutionary Algorithm for Parameter Optimization”. Evolutionary Computation, 1(1), pp. 1-23.
Ackley <- function(x, a = 20, b = 0.2, c = 2*pi) {
  d <- length(x)
  
  s <- -a * exp(-b*sqrt(sum(x^2)/d))
  t <- -exp(sum(cos(c*x))/d)
  
  return(s + t + a + exp(1))
}






#' Shubert
#'
#' @param x Numeric vector
#'
#' @return A scalar
#' @export
#'
#' @examples
Shubert <- function(x) {
  i <- c(1:5)
  v <- sapply(i, function(x) sum(i * cos((i+1)*x+i)))
  return( prod(v) )
}

