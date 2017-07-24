
re_detect <- function(x, pattern, quiet = FALSE) {
  if (length(pattern) > 1) {
    if (!quiet & length(x) > 1 & length(x) != length(pattern)) {
      message("`x` and `pattern` don´t have the same length!\n",
              "using the bigger one.")
    }
    if (length(x) > length(pattern)) {
      pattern <- rep_len(pattern,length(x))
    } else {
      x <- rep_len(x,length(pattern))
    }
    x <- map_lgl(re_detect, x, pattern)
  } else {
    is_na <- is.na(x)
    x[is_na] <- ""
    x <- grepl(pattern = pattern, x = x, perl = TRUE)
    x[is_na] <- NA
  }
  x
}







