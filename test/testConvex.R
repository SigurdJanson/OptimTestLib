setwd("..")
source("./R/Convex.R")
setwd("./test")



test_that("Bohachevsky: Optimum", {
  # Value at the optimum: f(opt)
  x <- rep(0, 2)
  expect_identical(Bohachevsky(x), 0)
  
  # Every other point in space returns a value > f(opt)
  lim <- c(-100, 100) # usually evaluated on the square x[i] ∈ [-100, 100]
  for(r in 1:100) {
    x <- runif(2, lim[1], lim[2])
    if (sum(x) == 0) x <- c(-0.1, 12)
    expect_gt(Bohachevsky(x), 0)
  }
})


test_that("Sphere: Optimum", {
  # Value at the optimum: f(opt)
  for(d in 2^(0:10)) {
    x <- rep(0, d)
    expect_identical(Sphere(x), 0)
  }

  # Every other point in space returns a value > f(opt)
  lim <- c(-5.12, 5.12) # usually evaluated on the square x[i] ∈ [-5.12, 5.12]
  for(r in 1:100) {
    x <- runif(2, lim[1], lim[2])
    if (sum(x) == 0) x <- c(-0.1, 12)
    expect_gt(Sphere(x), 0)
  }
})


test_that("SumSquare: Optimum", {
  # Value at the optimum: f(opt)
  for(d in 2^(0:10)) {
    x <- rep(0, d)
    expect_identical(SumSquare(x), 0)
  }
  
  # Every other point in space returns a value > f(opt)
  lim <- c(-10, 10) # usually evaluated on the square x[i] ∈ [-10, 10]
  for(r in 1:100) {
    x <- runif(2, lim[1], lim[2])
    if (sum(x) == 0) x <- c(-0.1, 12)
    expect_gt(SumSquare(x), 0)
  }
})

test_that("Schwefel223: Optimum", {
  # Value at the optimum: f(opt)
  for(d in 2^(0:10)) {
    x <- rep(0, d)
    expect_identical(Schwefel223(x), 0)
  }
  
  # Every other point in space returns a value > f(opt)
  lim <- c(-10, 10) # usually evaluated on the square x[i] ∈ [-10, 10]
  for(r in 1:100) {
    x <- runif(2, lim[1], lim[2])
    if (sum(x) == 0) x <- c(-0.1, 12)
    expect_gt(Schwefel223(x), 0)
  }
})
