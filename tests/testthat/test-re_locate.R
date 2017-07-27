context("re_locate")

# Needed: segregar melhor os testes


.res <- function(x, each = T) {
  x <- `if`(!each, x, rep(x,each = 2))
  matrix(x,
         ncol = 2,
         byrow = TRUE,
         dimnames = list(NULL,c("start","end")))
}

test_that("works as expected",{
  strings <- c("abcabc",
               "bab",
               "zzz",
               NA)
  expect_equal(
    re_locate(strings, "a"),
    .res(c(1,2,-1,NA))
  )

  expect_equal(re_locate(strings, c("a", "b")),
               .res(c(1L,1L,-1L,NA_integer_)))
  expect_equal(
    re_locate_all(strings, "a"),
    list(.res(c(1L,4L)),
         .res(2L),
         .res(-1L),
         .res(NA_integer_))
  )
  expect_equal(
    re_locate_all(strings, c("a", "b")),
    list(.res(c(1L,4L)),
         .res(c(1L,3L)),
         .res(-1L),
         .res(NA_integer_))
  )
})


