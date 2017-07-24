re_split <- function(x, pattern, n,na = NA) {
  if (length(x) == 1L) return(re_split_lst(x, pattern)[[1]])
  re_split_mat(x, pattern, na)
}

re_split_lst <- function(x, pattern) {
  x <- as_char(x)
  res <- strsplit(x, pattern, perl = TRUE)
  names(res) <- names(x)
  res
}

re_split_mat <- function(x, pattern, na = NA) {
  res <- re_split_lst(x, pattern)
  len <- max(map_int(length, res))
  res <- map_lst(chr_ensure_length, res,length = len, empty = na)
  n_row <- length(x)
  res <- unlist(res)
  res <- matrix(res,nrow = n_row, byrow = TRUE)
  rownames(res) <- names(x)
  res
}
