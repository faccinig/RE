context("re_detect")

test_that("special cases are correct", {
  expect_equal(re_detect(NA, "x"), NA)
  expect_equal(re_detect(character(), "x"), logical())
})

test_that("vectorised patterns work", {
  expect_equal(re_detect("ab", c("a", "b", "c")), c(T, T, F))
  expect_equal(re_detect(c("ca", "ab"), c("a", "c")), c(T, F))
})

test_that("modifiers work", {
  expect_false(re_detect("ab", "AB"))
  expect_true(re_detect("ab", "(?i)AB"))
  expect_true(re_detect("abc", "(?x)a b c"))
})

