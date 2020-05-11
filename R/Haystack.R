# Haystack functions having many local minima

# 2 Dimensions -----------------------


#' HolderTable
HolderTable <- function(x) {
  
  f1 <- sin(x[1])*cos(x[2])
  f2 <- exp( abs(1 - sqrt(x[1]^2 + x[2]^2) / pi) )
  
  return(-abs(fact1*fact2))
}



# N Dimensions -----------------------

#' Rastrigin
Rastrigin <- function(x) 10*length(x) + sum((x)^2 - 10*cos(1*pi*(x))) 

