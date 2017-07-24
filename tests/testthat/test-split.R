context("re_split")

x <- c("abc",
       "abcabc")
pattern <- "b"
res_v <- c("a", "c")
res_l <- list(c("a", "c"),
              c("a", "ca", "c"))
res_m <- matrix(c("a", "c" , NA,
                  "a", "ca", "c"),
                nrow = 2L, byrow = TRUE)

test_that("works as expected", {
  expect_identical(re_split(x, pattern), res_m)
  expect_identical(re_split_mat(x, pattern), res_m)
  expect_identical(re_split_lst(x, pattern), res_l)
})

test_that("single value are correct", {
  expect_identical(re_split(x[2], pattern), res_l[[2]])
  expect_identical(re_split_mat(x[2], pattern), res_m[2,,drop = FALSE])
  expect_identical(re_split_lst(x[2], pattern), res_l[2])
})

test_that("named vector keep its names", {
  nm <- c("a" ,"b")
  names(x) <- nm
  names(res_l) <- nm
  rownames(res_m) <- nm
  expect_identical(re_split(x, pattern), res_m)
  expect_identical(re_split_mat(x, pattern), res_m)
  expect_identical(re_split_lst(x, pattern), res_l)
})

test_that("NAs are treated right", {
  res_v <- NA_character_
  res_l <- list(res_v)
  res_m <- matrix(res_v,
                  nrow = 1L, byrow = TRUE)
  expect_identical(re_split(NA, pattern), res_v)
  expect_identical(re_split_mat(NA, pattern), res_m)
  expect_identical(re_split_lst(NA, pattern), res_l)
})
