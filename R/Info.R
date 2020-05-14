# Choose a test function ot get information about one
# For later usage: do.call("rep", list(x = 3, time = 5))



GetTest <- function(dim, cont, diff, convex, modals, sep, scale, random, parametric) {
  #TODO
}


DescribeTest <- function(name) {
  # PRECONDITIONS
  if(missing(Name)) stop("Argument 'Name' is missing.")
  if (is.function(Name)) Name <- as.character(substitute(mean))

  df <- read.csv("./data/testfuncdb.csv")
    
}




GetOptima <- function(Name, Dim = 2) {
  # PRECONDITIONS
  if(missing(Name)) stop("Argument 'Name' is missing.")
  if (is.function(Name)) Name <- as.character(substitute(mean))
  
  # RUN
  # Identify rows with requested function
  df <- read.csv("./data/testfuncdb.csv")
  r <- grep(Name, df$Name)
  if(length(r) == 0) return(NA)
  
  # Which tests work in the selected dimensions
  if (!is.na(Dim)) {
    rd <- which(is.na(df$Dimensions) | df$Dimensions == Dim)
    r <- intersect(r, rd)
  }
  
  # 
  Result <- list()
  for(row in r) {
    values    <- as.character(df[row, "GlobalOptValue"])
    values    <- eval(parse( text=values ))
    locations <- as.character(df[row, "GlobalOptLocation"])
    locations <- eval(parse( text=locations ))
    Addon <- list(Location = locations, Value = values)
    Result <- c(Result, list(Addon))
    names(Result)[length(Result)] <- as.character(df[row, "Function"])
  }
  
  return(Result)
}
#GetOptima("Keane")
