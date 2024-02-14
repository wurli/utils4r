as_numeric_strict <- function(x) {
  x_ok <- TRUE
  
  suppressWarnings(withCallingHandlers(
    out <- as.numeric(x), 
    warning = function(w) x_ok <<- FALSE
  ))
  
  if (!x_ok) {
    bad_elements <- x[!is.na(x) & is.na(out)]
  
    cli::cli_abort(
      c("Cannot coerce vector to {.cls numeric}",
        i = "Check element{?s} {.val {bad_elements}}"),
      call = rlang::caller_call()
    )
  }

  out
}
