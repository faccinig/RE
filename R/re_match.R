
re_match_lst <- function(x, pattern, na = NA_character_) {
  res <- re_match_all_lst(x, pattern, na)
  map_lst(`[[`, res,1L)
}

re_match_all_lst <- function(x, pattern, na = NA_character_) {
  if (is.na(pattern)) return(list(list(NA_character_)))
  if (length(x) == 0L) return(list(list(character())))
  nas <- is.na(x)
  x[nas] <- ""
  .simplify_match <- function(.m) {
    # l <- `if`(all, seq_along(as.vector(.m)), 1L)
    l <- seq_along(as.vector(.m))
    map_lst(function(.i) {
      mat_start = as.vector(.m)[.i]
      mat_end = mat_start + attr(.m,"match.length")[.i] - 1L
      cap_start = attr(.m,"capture.start")[.i,]
      cap_end = cap_start + attr(.m,"capture.length")[.i,] - 1L
      list(
        start = c(mat_start,cap_start),
        end = c(mat_end,cap_end))
    }, l)
  }
  .extract_match <- function(.s, .m) {
    map_lst(function(.m) {
      if (.m$start[1] == -1) {
        res <- rep_along(na,.m$start)
      } else {
        res <- map_chr(substr,
                       .s,.m$start,.m$end)
      }
      if(any(names(.m$start) != "")) setNames(res, names(.m$start))
      res
    }, .m)
  }

  res <- gregexpr(pattern, x,useBytes = FALSE,perl = TRUE)
  res <- map_lst(.simplify_match, res)
  res <- map_lst(.extract_match,x,res)
  map_if(function(.m) {
    list(as.character(rep_along(na,.m[[1]])))
  }, nas, res)
}

re_match_all_mat <- function(x, pattern, na = NA_character_) {
  res <- re_match_all_lst(x, pattern, na)
  lst2matrix(res)
}

re_match_mat <- function(x, pattern, na = NA_character_) {
  res <- re_match_lst(x, pattern, na)
  lst2matrix(res)
}


re_match <- function(x, pattern, na = NA) {
  if (length(x) <= 1) return(re_match_lst(x, pattern, na)[[1]])
  else return(re_match_mat(x, pattern, na))
}

re_match_all <- re_match_all_mat






#
# source("R/little_purrr.R")
# source("R/purrr_plus.R")
#
# x <- c(" 219 733 8965",
#        "329-293-8753 ",
#        "banana",
#        "239 923 8115 and 842 566 4692",
#        "654555")
# pattern <- "(?<one>[0-9]{3})?[- .]?(?<two>[0-9]{3})?[- .]?(?<three>[0-9]{4})\\s?$"
# pattern <- "([0-9]{3})[- .]?([0-9]{3})[- .]?([0-9]{4})?"
#
# t <- re_match_lst(x,pattern)
#
# lst2matrix(m)
#
# m <- map1_lst(m,.simplify_match,all = T)