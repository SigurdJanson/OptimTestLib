# Random
# Test functions with a random component



#' Quartic
#' Optimisation test function with a stochastic component.
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' @source Function 100 in Jamil, M., & Yang, X. S. (2013). A literature survey of benchmark functions for global optimisation problems. International Journal of Mathematical Modelling and Numerical Optimisation, 4(2), 150. https://doi.org/10.1504/ijmmno.2013.055204
#' @export
Quartic <- function(x) {
  d <- 1:length(x)
  e <- runif(1, max = 1-sqrt(.Machine$double.eps))
  return( sum(d * x^4) + e )
}



#' XinShe
#' Optimisation test function with a stochastic component.
#' @param x Numeric vector representing a coordinate in a parameter space
#' @return A scalar
#' @source Function 169 in Jamil, M., & Yang, X. S. (2013). A literature survey of benchmark functions for global optimisation problems. International Journal of Mathematical Modelling and Numerical Optimisation, 4(2), 150. https://doi.org/10.1504/ijmmno.2013.055204
#' @export
XinShe <- function(x) {
  d <- length(x)
  return( sum(runif(d) * abs(x)^d) )
}


