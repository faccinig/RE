context("re_replace")

test_that("basic replacement works", {
  expect_equal(re_replace_all("abababa", "ba", "BA"), "aBABABA")
  expect_equal(re_replace("abababa", "ba", "BA"), "aBAbaba")
  expect_equal(re_replace_all("abcde", "(b)(c)(d)", "\\1"), "abe")
  expect_equal(re_replace("abcde", "(b)(c)(d)", "\\2"), "ace")
})

test_that("vectorised patterns work", {
  x <- "aba"
  pattern     <- c("a", "b")
  replacement <- 1:3
  res     <- c("1ba", "a2a", "3ba")
  res_all <- c("1b1", "a2a", "3b3")
  expect_equal(re_replace(x, pattern,replacement),res)
  expect_equal(re_replace_all(x, pattern,replacement),res_all)
})

test_that("can replace multiple matches", {
  x <- c("aba", "bab")
  pattern <- c("a" = "1", "b" = "2")
  res     <- c("12a", "21b")
  res_all <- c("121", "212")
  expect_equal(re_replace(x, pattern),res)
  expect_equal(re_replace_all(x, pattern),res_all)
})

test_that("special cases are correct", {
  expect_equal(re_replace("xyz", "x", NA), NA_character_)
  expect_equal(re_replace_all("xyz", "x", NA), NA_character_)
  expect_equal(re_replace(character(), "x", "y"), character())
  expect_equal(re_replace_all(character(), "x", "y"), character())
})

