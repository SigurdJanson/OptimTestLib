setwd("..")
source("./R/Haystack.R")
setwd("./test")



test_that("HolderTable1: Optimum", {
  # Value at the optimum: f(opt)
  x0 <- c(9.646168, 9.646168)
  x <- x0 * c(+1,+1)
  expect_equal(HolderTable1(x), -26.920336, tol = 5e-6)
  x <- x0 * c(-1,+1)
  expect_equal(HolderTable1(x), -26.920336, tol = 5e-6)
  x <- x0 * c(+1,-1)
  expect_equal(HolderTable1(x), -26.920336, tol = 5e-6)
  x <- x0 * c(-1,-1)
  expect_equal(HolderTable1(x), -26.920336, tol = 5e-6)
  
  # Every other point in space returns a value > f(opt)
  lim <- c(-10, 10) # usually evaluated on the square x[i] ∈ [-10, 10]
  for(r in 1:100) {
    x <- runif(2, lim[1], lim[2])
    if (abs(x[1]) == x0[1] || abs(x[2]) == x0[2]) x <- c(-0.1, 12)
    expect_gt(HolderTable1(x), -26.920336)
  }
})



test_that("HolderTable2: Optimum", {
  # Value at the optimum: f(opt)
  x0 <- c(8.055023472141116, 9.664590028909654)
  x <- x0 * c(+1,+1)
  expect_equal(HolderTable2(x), -19.20850, tol = 5e-6)
  x <- x0 * c(-1,+1)
  expect_equal(HolderTable2(x), -19.20850, tol = 5e-6)
  x <- x0 * c(+1,-1)
  expect_equal(HolderTable2(x), -19.20850, tol = 5e-6)
  x <- x0 * c(-1,-1)
  expect_equal(HolderTable2(x), -19.20850, tol = 5e-6)
  
  # Every other point in space returns a value > f(opt)
  lim <- c(-10, 10) # usually evaluated on the square x[i] ∈ [-10, 10]
  for(r in 1:100) {
    x <- runif(2, lim[1], lim[2])
    if (abs(x[1]) == x0[1] || abs(x[2]) == x0[2]) x <- c(-0.1, 12)
    expect_gt(HolderTable2(x), -19.20850)
  }
})

test_that("Rastrigin: Optimum", {
  # Value at the optimum: f(opt)
  for(d in 2^(0:10)) {
    x <- rep(0, d)
    expect_identical(Rastrigin(x), 0)
  }
  
  # Every other point in space returns a value > f(opt)
  lim <- c(-5.12, 5.12) # usually evaluated on the square x[i] ∈ [-5.12, 5.12]
  for(r in 1:100) {
    x <- runif(2, lim[1], lim[2])
    if (sum(x) == 0) x <- c(-0.1, 12)
    expect_gt(Rastrigin(x), 0)
  }
})


test_that("Schwefel: Optimum", {
  # # Value at the optimum: f(opt)
  # for(d in 2^(0:10)) {
  #   x <- rep(0, d)
  #   expect_identical(Sphere(x), 0)
  # }
  # 
  # # Every other point in space returns a value > f(opt)
  # lim <- c(-5.12, 5.12) # usually evaluated on the square x[i] ∈ [-5.12, 5.12]
  # for(r in 1:100) {
  #   x <- runif(2, lim[1], lim[2])
  #   if (sum(x) == 0) x <- c(-0.1, 12)
  #   expect_gt(Sphere(x), 0)
  # }
})
