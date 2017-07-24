context("chr_pad")

test_that("works as expected", {
  x <- c("a","ab","abc")
  width <- 3L
  pad <- "*"
  res_l <- c("**a","*ab","abc")
  res_r <- c("a**","ab*","abc")
  res_b <- c("*a*","*ab","abc")
  expect_equal(chr_pad(x,width,side = "left",pad), res_l)
  expect_equal(chr_pad(x,width,side = "right",pad), res_r)
  expect_equal(chr_pad(x,width,side = "both",pad), res_b)
})

test_that("vectorised width work", {
  x <- "a"
  width <- 1:3
  pad <- "*"
  res_l <- c("a","*a","**a")
  res_r <- c("a","a*","a**")
  res_b <- c("a","*a","*a*")
  expect_equal(chr_pad(x,width,side = "left",pad), res_l)
  expect_equal(chr_pad(x,width,side = "right",pad), res_r)
  expect_equal(chr_pad(x,width,side = "both",pad), res_b)
  x <- c("a","b")
  width <- 2:3
  res_l <- c("*a","**b")
  res_r <- c("a*","b**")
  res_b <- c("*a","*b*")
  expect_equal(chr_pad(x,width,side = "left",pad), res_l)
  expect_equal(chr_pad(x,width,side = "right",pad), res_r)
  expect_equal(chr_pad(x,width,side = "both",pad), res_b)
})

test_that("vectorised pad work", {
  pad <- c("","*","-")
  x <- "a"
  width <- 3
  res_l <- c("a","**a","--a")
  res_r <- c("a","a**","a--")
  res_b <- c("a","*a*","-a-")
  expect_equal(chr_pad(x,width,side = "left",pad), res_l)
  expect_equal(chr_pad(x,width,side = "right",pad), res_r)
  expect_equal(chr_pad(x,width,side = "both",pad), res_b)
})

