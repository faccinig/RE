map_lst <- function(.f, .x, ...) {
  .f <- match.fun(.f)
  res <- suppressWarnings(mapply(FUN = .f,
                                 .x,
                                 ...,
                                 SIMPLIFY = FALSE,
                                 USE.NAMES = FALSE))
  if (is_named(.x)) {
    names(res) <- rep_len(names(.x),length(res))
  }
  res
}
.map_factory <- function(.mode){
  function(.f, .x, ...) {
    .f <- match.fun(.f)
    res <- suppressWarnings(mapply(FUN = .f,
                                   .x,
                                   ...,
                                   SIMPLIFY = FALSE,
                                   USE.NAMES = FALSE))
    res <- as.vector(res, .mode)
    if (is_named(.x)) {
      names(res) <- rep_len(names(.x),length(res))
    }
    res
  }
}
map_lgl  <- .map_factory("logical"  )
map_int  <- .map_factory("integer"  )
map_dbl  <- .map_factory("double"   )
map_chr  <- .map_factory("character")
map_cpl  <- .map_factory("complex"  )

map_if <- function(.f, .p, .x, ...) {
  if (!is_logical(.p)) .p <- map_lgl(.p, .x)
  .x[.p] <- map_lst(.f, .x[.p], ...)
  .x
}