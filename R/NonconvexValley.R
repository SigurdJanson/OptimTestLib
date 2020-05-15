


#' Bohachevsky1
#' Computes first Bohachevsky function at point `x`.
#' @param x Vector with two positions
#' @return A scalar
#' @export
#' @examples Bohachevsky1(c(1, 1))
Bohachevsky1 <- function( x ) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. Bohachevsky1 is defined for exactly 2 dimensions.")
  
  s3 <- -0.3 * cos(3*pi*x[1])
  s4 <- -0.4 * cos(4*pi*x[2])
  z <- x[1]^2 + 2*x[2]^2 + s3 + s4 + 0.7
  
  return(z)
}

#' Bohachevsky2
#' Computes 2nd Bohachevsky function at point `x`.
#' @param x Vector with two positions
#' @return A scalar
#' @export
#' @examples Bohachevsky2(c(1, 1))
Bohachevsky2 <- function( x ) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. Bohachevsky2 is defined for exactly 2 dimensions.")
  
  s3 <- -0.3 * cos(3*pi*x[1])
  s4 <- -0.4 * cos(4*pi*x[2])
  z <- x[1]^2 + 2*x[2]^2 + s3 * s4 + 0.3
  
  return(z)
}


#' Bohachevsky3
#' Computes 3rd Bohachevsky function at point `x`.
#' @param x Vector with two positions
#' @return A scalar
#' @export
#' @examples Bohachevsky3(c(1, 1))
Bohachevsky3 <- function( x ) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. Bohachevsky3 is defined for exactly 2 dimensions.")
  
  s3 <- 3*pi*x[1]
  s4 <- 4*pi*x[2]
  z <- x[1]^2 + 2*x[2]^2 -0.3 * cos( s3 + s4 ) + 0.3
  
  return(z)
}
