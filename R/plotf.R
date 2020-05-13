# plot

#' outer2
#' The outer product of the arrays `X` and `Y` is the array `A` with dimension 
#' `c(dim(X), dim(Y))` where element `A[c(arrayindex.x, arrayindex.y)] = 
#' FUN(X[arrayindex.x], Y[arrayindex.y], ...)`. This is a patch to the [outer] 
#' function that can handle non-vectorized functions as well as vectorized ones.
#' @param X,Y First and second arguments for function `FUN.` 
#' Typically a vector or array.
#' @param FUN a function to use on the outer products, found via [match.fun] 
#' (except for the special case "`*`").
#' @param ... optional arguments to be passed to `FUN`.
#' @param Vectorized 
#' @return A matrix
#' @seealso [outer]
#' @source https://tolstoy.newcastle.edu.au/R/help/05/10/14725.html
#' @author Tony Plate
#' @examples
outer2 <- function(X, Y, FUN = "*", ..., Vectorized = FALSE) {
  no.nx <- is.null(nx <- dimnames(X <- as.array(X)))
  dX <- dim(X)
  no.ny <- is.null(ny <- dimnames(Y <- as.array(Y)))
  dY <- dim(Y)
  if (is.character(FUN) && FUN == "*") {
    robj <- as.vector(X) %*% t(as.vector(Y))
    dim(robj) <- c(dX, dY)
  } else {
    FUN <- match.fun(FUN)
    Y <- rep(Y, rep.int(length(X), length(Y)))
    if (length(X) > 0)
      X <- rep(X, times = ceiling(length(Y)/length(X)))
    if (Vectorized)
      robj <- FUN(X, Y, ...)
    else
      robj <- mapply(FUN, X, Y, MoreArgs=list(...))
    dim(robj) <- c(dX, dY)
  }
  if (no.nx)
    nx <- vector("list", length(dX))
  else if (no.ny)
    ny <- vector("list", length(dY))
  if (!(no.nx && no.ny))
    dimnames(robj) <- c(nx, ny)
  robj
}


#'plotf1
#'
#' @param f 
#' @param from 
#' @param to 
#' @param ... 
#'
#' @return Directly plots the function results
#' @export
#' @examples
plotf1 <- function(f, from = -5, to = 5, ...) {
  steps <- 200
  # 
  se <- c(from, to)
  dim(se) <- c(length(from), 2)
  # Create a n-dim plane
  x <- apply(se, 1, function(i) seq(i[1], i[2], length.out = steps))

  # Feed the plane into the function
  fx <- apply(x, 1, f, ...)
  
  # Plot the results
  plot(1:length(fx), fx, type = "l")
}
# source("./R/Haystack.R")
# p <- plotf1(function(x) 10*length(x) + sum((x)^2 - 10*cos(1*pi*(x))),
#             from = rep(-10,2), to = rep(10,2))



#' plotf2
#'
#' @param f 
#' @param from Numeric vector with 2 positions
#' @param to  Numeric vector with 2 positions
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plotf2 <- function(f, from = c(-5, -5), to = c(5, 5), ...) {
  # PRECONDITIONS
  require(plot3D)
  steps <- 200
  # Run
  x <- seq(from[1], to[1], length.out = steps)
  y <- seq(from[2], to[2], length.out = steps)
  z <- outer2(x, y, function(a, b) f(c(a, b)), ...)
  persp3D(x, y, z = z)
}
p <- plotf2(function(x) 10*length(x) + sum((x)^2 - 10*cos(1*pi*(x))))



plotfc <- function(f, from = c(-5, -5), to = c(5, 5), ...) {
  # PRECONDITIONS
  require(plot3D)
  steps <- 200
  # Run
  x <- seq(from[1], to[1], length.out = steps)
  y <- seq(from[2], to[2], length.out = steps)
  z <- outer2(x, y, function(a, b) f(c(a, b)), ...)
  contour(z = z)
}
#p <- plotfc(function(x) 10*length(x) + sum((x)^2 - 10*cos(1*pi*(x))))


plotf <- function(f, type = c("one", "two", "contour"), plane = NULL, 
                  range = 5, center = 0,
                  ...) {
  type <- match.arg(type)
  switch(type,
         one = plotf1(x, ...),
         two = plotf2(x, ...),
         contour = plotfc(x, ...))
}

