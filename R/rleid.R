#' Run length encoding across multiple atomic vectors
#'
#' Inspired by a post by Mike FC on Fosstodon: 
#' https://fosstodon.org/@coolbutuseless/112144601383918334
#'
#' @param ... multiple atomic vectors of the same length
#'
#' @return Integer vector of rleids for each unique set of values as they appear
#' @noRd
#'
#' @examples
#' v1 <- c(1, NA, 1, 2, 2, 2)
#' v2 <- c(1, 1, 2, 2, 2, NA)
#' rleid(v1, v2) == c(1, 2, 3, 4, 4, 5)
rleid <- function(...) {
  ll <- list(...)

  stopifnot(exprs = {
    length(ll) > 0 # at least one vector
    length(ll[[1]]) > 0 # the first vector is not length = 0
    length(unique(lengths(ll))) == 1  # All vectors are the same length
  })
  
  changes <- lapply(ll, function(v) {
    v_lead <- v[-1]
    v      <- v[-length(v)]

    na_change  <- is.na(v) != is.na(v_lead)
    val_change <- v != v_lead
    
    ifelse(is.na(val_change), na_change, val_change)
  })

  cumsum(c(TRUE, Reduce(`|`, changes)))
}
