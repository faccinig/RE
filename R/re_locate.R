

re_locate_all <- function(x, pattern) {
  .simplify_match <- function(.m) {
    if (is.na(.m)) {
      start <- NA_integer_
      end <- NA_integer_
    } else if (.m > 0L) {
      start <- as.integer(.m)
      end <- attr(.m,"match.length") + start - 1L
    } else {
      start <- -1L
      end <- -1L
    }
    cbind(
      start,
      end
    )
  }
  .gregexpr <- function(x, pattern) {
    if (is.na(pattern)) return(NA)
    gregexpr(pattern, x,useBytes = FALSE,perl = TRUE)
  }
  is_na <- is.na(x)
  x[is_na] <- ""
  if (length(pattern) > 1) {
    res <- map_lst(.gregexpr, x, pattern)
    res <- map_lst(`[[`,res,1L)
  } else res <- .gregexpr(x, pattern)
  res <- map_if(function(x) NA,
                is_na,
                res)
  map_lst(.simplify_match, res)
}

re_locate <- function(x, pattern) {
  res <- re_locate_all(x, pattern)
  res <- map_lst(function(.x) .x[1,],res)
  do.call(rbind, res)
}

