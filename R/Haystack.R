# Haystack functions having many local minima



# 1 Dimension -----------------------

#' GramacyLee
#' Gramacy & Lee Function
#' @param x Numeric vector of length 1 representing a coordinate in a parameter space
#' @return A scalar
#' @source 
#' @export
GramacyLee <- function(x) {
  if(length(x) != 1) 
    stop("Wrong length of 'x'. GramacyLee is defined for exactly 1 dimension.")

  return( sin(10*pi*x)/(2*x) + (x-1)^4 )
}


# 2 Dimensions -----------------------


#' Ackley3
#' 
#' @param x Numeric vector or length 2 representing a coordinate in a parameter space
#' @return A scalar
#' @source 
#' @export
Ackley3 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. Ackley3 is defined for exactly 2 dimensions.")

  s1 <- -200 * exp(0.02*sqrt(x[1]^2 + x[2]^2))
  s2 <- 5 * exp(cos(3*x[1])+sin(3*x[2]))
  return( s1 + s2 )
}


#' Keane
#' 
#' @param x Numeric vector or length 2 representing a coordinate in a parameter space
#' @return A scalar
#' @source 
#' @export
Keane <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. Keane is defined for exactly 2 dimensions.")

  n <- sin(x[1]-x[2])^2 * sin(x[1]+x[2])^2
  return( n / sqrt(x[1]^2+x[2]^2) )
}


#' HolderTable1
#'
#' @param x Numeric vector or length 2 representing a coordinate in a parameter space
#' @return A scalar
#' @source 
#' @export
HolderTable1 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. HolderTable1 is defined for exactly 2 dimensions.")
  
  f1 <- cos(x[1])*cos(x[2])
  f2 <- exp( abs(1 - sqrt(x[1]^2 + x[2]^2) / pi) )
  
  return(-abs(f1*f2))
}


#' HolderTable2
#'
#' @param x Numeric vector or length 2 representing a coordinate in a parameter space
#' @return A scalar
#' @source 
#' @export
HolderTable2 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. HolderTable2 is defined for exactly 2 dimensions.")
  
  f1 <- sin(x[1])*cos(x[2])
  f2 <- exp( abs(1 - sqrt(x[1]^2 + x[2]^2) / pi) )
  
  return(-abs(f1*f2))
}


#' SchafferF6
#' Sine evelop sine wave function.
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' @note This function deviated from Function 136 in Jamil & Yang (2013).
#' The publicationlacks clarity there and is not equal to the original source
#' which is Schaffer et al. (1989).
#' @references 
#' Jamil, M., & Yang, X. S. (2013). A literature survey of benchmark functions for global optimisation problems. International Journal of Mathematical Modelling and Numerical Optimisation, 4(2), 150. https://doi.org/10.1504/ijmmno.2013.055204
#' Schaffer, J., Caruana, R., Eshelman, L., & Das, R. (1989). A Study of Control Parameters Affecting Online Performance of Genetic Algorithms for Function Optimization. Third international Conference on Genetic Algorithms, 51–60.
#' @export
SchafferF6 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. ShafferF6 is defined for exactly 2 dimensions.")

  xsys <- x[1]^2 + x[2]^2
  return( 0.5 + (sin(sqrt(xsys))-0.5) / (1 + 0.001*xsys)^2 )
}


# N Dimensions -----------------------

#' Rastrigin
#'
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' @source 
#' @export
Rastrigin <- function(x) {
  10*length(x) + sum((x)^2 - 10*cos(1*pi*(x)))
}


#' Schwefel
#' 
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' ---TODO: not sure about this anymore - conflicting sources do not help
Schwefel <- function(x) {
  418.9829*length(x) - sum( x*sin(sqrt(abs(x))) )
}


#' Ackley
#' 
#' @param x Numeric vector representing a coordinate in a parameter space
#' @param a = default value 20
#' @param b =  default value 0.2
#' @param c =  default value 2*pi
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
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' @export
Shubert <- function(x) {
  i <- c(1:5)
  v <- sapply(i, function(x) sum(i * cos((i+1)*x+i)))
  return( prod(v) )
}

