interleave <- function(x, y) {
  stopifnot(length(y) %in% c(1L, length(x) - 1L))
  
  if (length(y) == 1L)
    y <- rep(y, length(x))
  
  out <- lapply(
    seq_len(length(x) - 1L), 
    function(i) c(x[i], y[i])
  )
  
  out <- append(out, x[length(x)])
  
  unlist(out, use.names = FALSE, recursive = FALSE)
}

rep_interleave <- function(x, y, n) {
  rep(c(y, x), n)[-seq_along(y)]
}

staggar <- function(...) {
  combined <- tryCatch(
    cbind(...), 
    warning = function(w) stop("Can't combine inputs")
  )
  do.call(c, asplit(cbind(...), 1))
}
