context("re_match")

x <- c(" 219 733 8965",
       "329-293-8753 ",
       "banana",
       "239 923 8115 and 842 566 4692",
       "654555")
pattern <- "([0-9]{3})[- .]?([0-9]{3})[- .]?([0-9]{4})?"

res_l <- list(
  c("219 733 8965","219" ,"733" ,"8965"),
  c("329-293-8753","329" ,"293" ,"8753"),
  c(NA_character_,NA_character_,NA_character_,NA_character_),
  c("239 923 8115","239" ,"923" ,"8115"),
  c("654555","654" ,"555" ,""))
res_m <- Reduce(rbind,res_l,NULL)



test_that("works as expected", {
  expect_equal(re_match(x[1],pattern), res_l[[1]])
  expect_equal(re_match(x,pattern), res_m)
  expect_equal(re_match_lst(x,pattern), res_l)
  expect_equal(re_match_mat(x,pattern), res_m)
})

test_that("named vector works", {
  nm <- p("string",1:5)
  names(x) <- nm
  names(res_l) <- nm
  rownames(res_m) <- nm
  expect_equal(re_match(x[1],pattern), res_l[[1]])
  expect_equal(re_match(x,pattern), res_m)
  expect_equal(re_match_lst(x,pattern), res_l)
  expect_equal(re_match_mat(x,pattern), res_m)
})


test_that("special cases are correct", {
  expect_equal(re_match(NA,"pattern"), NA_character_)
  expect_equal(re_match_lst(NA,"pattern"), list(NA_character_))
  expect_equal(re_match_mat(NA,"pattern"), matrix(NA_character_))

  expect_equal(re_match("x", NA), NA_character_)
  expect_equal(re_match_lst("x", NA), list(NA_character_))
  expect_equal(re_match_mat("x", NA), matrix(NA_character_))

  expect_equal(re_match(character(),"pattern"), character())
  expect_equal(re_match_lst(character(),"pattern"), list(character()))
  expect_equal(re_match_mat(character(),"pattern"), matrix(character(),nrow = 1L))
})



# Match All!!!!

test_that("re_match_all works as expected", {
  res_l <- list(
    list(c("219 733 8965","219" ,"733" ,"8965")),
    list(c("329-293-8753","329" ,"293" ,"8753")),
    list(c(NA_character_,NA_character_,NA_character_,NA_character_)),
    list(c("239 923 8115","239" ,"923" ,"8115"),
         c("842 566 4692","842" ,"566","4692")),
    list(c("654555","654" ,"555" ,"")))
  res_m <- lst2matrix(res_l)
  # expect_equal(re_match_all(x[4],pattern), res_m[4:5])
  expect_equal(re_match_all(x,pattern), res_m)
  expect_equal(re_match_all_lst(x,pattern), res_l)
  expect_equal(re_match_all_mat(x,pattern), res_m)
})