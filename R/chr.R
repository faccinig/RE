chr_ensure_length <- function(x, length, empty = "") {
  len <- length - length(x)
  if (len < 0) return(x[1:length])
  return(c(x,rep.int(empty,len)))
}


chr_inverse <- function(x) {
  if (length(x) == 0L ) return(character())
  is_na <- is.na(x)
  x[is_na] <- ""
  res <- strsplit(x, NULL)
  res <- map_lst(rev, res)
  res <- map_chr(paste0,
                 res, collapse = "")
  res[is_na] <- NA_character_
  return(res)
}

chr_rep <- function(x, times = 1L) {
  map_chr(function(y,t) p(rep.int(y,t), collapse = ""),
          x,times)
}


p  <- function(..., sep = "" , collapse = NULL) paste(...,
                                                      sep = sep,
                                                      collapse = collapse)
ps <- function(..., sep = " ", collapse = NULL) paste(...,
                                                      sep = sep,
                                                      collapse = collapse)
pc <- function(..., sep = "" , collapse = ""  ) paste(...,
                                                      sep = sep,
                                                      collapse = collapse)


chr_pad <- function(x,width = 1L, side = "left", pad = " ") {
  width <- as.integer(width)
  row_width <- nchar(x, type = "chars", keepNA = FALSE)
  width <- width - row_width
  width <- ifelse(width > 0L, width, 0L)
  if (side == "both") {
    right <- as.integer(width/2)
    left <- width - right
    right <- chr_rep(pad,right)
    left <- chr_rep(pad,left)
    return(p(left,x,right))
  } else {
    pad <- chr_rep(pad,width)
    if (side == "left") return(p(pad,x))
    else return(p(x,pad))
  }
}

chr <- function(...) {
  vec <- deparse(substitute(alist(...)))
  vec <- pc(vec)
  vec <- substr(vec,7,nchar(vec)-1)
  vec <- re_split(vec,",")
  vec <- re_trim(vec)
  vec
}

chr_c <- function(...) {
  vec <- chr(...)
  vec <- p("c(",
           p('"',vec, '"', collapse = " ,"),
           ")")
  cat(vec)
  invisible(vec)
}


as_char <- function(x) {
  res <- as.character(x)
  names(res) <- names(x)
  res
}
