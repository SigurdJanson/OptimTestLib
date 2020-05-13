# plot

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



plotf2 <- function(f, from = -5, to = 5, ...) {
  require(plot3D)
  # TBD
}

plotfc <- function(f, from = -5, to = 5, ...) {
  require(plot3D)
  # TBD
}


plotf <- function(f, type = c("one", "two", "contour"), plane = NULL, 
                  range = 5, center = 0,
                  ...) {
  type <- match.arg(type)
  switch(type,
         one = plotf1(x, ...),
         two = plotf2(x, ...),
         contour = plotfc(x, ...))
}

