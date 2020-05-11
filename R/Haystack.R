# Haystack functions having many local minima

# 2 Dimensions -----------------------

#' HolderTable1
HolderTable1 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. HolderTable1 is defined for exactly 2 dimensions.")
  
  f1 <- cos(x[1])*cos(x[2])
  f2 <- exp( abs(1 - sqrt(x[1]^2 + x[2]^2) / pi) )
  
  return(-abs(f1*f2))
}


#' HolderTable2
HolderTable2 <- function(x) {
  if(length(x) != 2) 
    stop("Wrong length of 'x'. HolderTable2 is defined for exactly 2 dimensions.")
  
  f1 <- sin(x[1])*cos(x[2])
  f2 <- exp( abs(1 - sqrt(x[1]^2 + x[2]^2) / pi) )
  
  return(-abs(f1*f2))
}



# N Dimensions -----------------------

#' Rastrigin
Rastrigin <- function(x) 10*length(x) + sum((x)^2 - 10*cos(1*pi*(x))) 


#' Schwefel
#' ---TODO: not sure about this anymore - conflicting sources do not help
Schwefel <- function(x) 418.9829*length(x) - sum( x*sin(sqrt(abs(x))) )

