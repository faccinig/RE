if_na <- function(a, b) {
  a[is.na(a)] <- b
  a
}

is_named <- function(x) !is.null(names(x))
is_logical <- function(x) class(x) == "logical"

reduce2 <- function(.init,.x, .y, .f, ...) {
  if (length(.x) != length(.y)) stop("not same length!")
  res <- .init
  # .f <- match.fun(.f)
  for (i in seq_along(.x)) {
    res <- .f(res,.x[[i]],.y[[i]],...)
  }
  res
}

rep_along <- function(x, vec) {
  len <- length(vec)
  rep_len(x,len)
}

lst2matrix <- function(...) {

  .named_matrix <- function(l) {
    m <- l[[1]]
    if (!is.null(names(l)) & is.null(rownames(m))) {
      rownames(m) <- rep_len(names(l),nrow(m))
    }
    m
  }

  lst <- list(...)
  if(length(lst) == 1L) lst <- lst[[1]]
  lst <- map_if(lst2matrix,
                is.list, lst)
  lst <- map_if(function(.v) matrix(.v,nrow = 1L),
                Negate(is.matrix), lst)
  res <- .named_matrix(lst[1])
  l <- length(lst)
  if(l > 1 ) {
    for (i in 2:l) {
      res <- rbind(res,
                   .named_matrix(lst[i]))
    }
  }
  res
}

