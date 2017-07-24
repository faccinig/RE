.re_replace <- function(replace_func) {
  fun <- function(x,pattern, replacement) {
    replace_func(pattern = pattern,
                 replacement = replacement,
                 x = x, perl = TRUE)
  }
  function(x,pattern, replacement) {
    if (is_named(pattern)) {
      replacement <- pattern
      pattern <- names(pattern)
      x <- reduce2(x,pattern,replacement,fun)
    } else if (length(pattern) > 1| length(replacement) > 1) {
      x <- map_chr(fun, x,pattern, replacement)
    } else {
      x <- fun(x,pattern, replacement)
    }
    x
  }
}
re_replace <- .re_replace(sub)
re_replace_all <- .re_replace(gsub)