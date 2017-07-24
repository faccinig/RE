context("re_trim")

test_that("trimming removes spaces", {
  expect_equal(re_trim("abc   "),   "abc")
  expect_equal(re_trim("   abc"),   "abc")
  expect_equal(re_trim("  abc   "), "abc")
})

test_that("trimming removes tabs", {
  expect_equal(re_trim("abc\t"),   "abc")
  expect_equal(re_trim("\tabc"),   "abc")
  expect_equal(re_trim("\tabc\t"), "abc")
})

test_that("side trim restricts trimming", {
  expect_equal(re_trim_left(" abc "),  "abc ")
  expect_equal(re_trim_right(" abc "), " abc")
})




