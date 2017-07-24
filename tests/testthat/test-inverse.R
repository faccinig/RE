context("chr_inverse")


test_that("special cases are correct", {
  expect_equal(chr_inverse(NA), NA_character_)
  expect_equal(chr_inverse(character()), character())
})

test_that("works as expected", {
  expect_equal(chr_inverse("ABC"),"CBA")
  expect_equal(chr_inverse(c("ABC","XYZ")),
               c("CBA", "ZYX"))
})



