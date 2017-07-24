context("chr_ensure_length")

test_that("works as expected", {
  expect_equal(chr_ensure_length(c("1","12"),3),
               c("1","12",""))
})