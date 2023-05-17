#' Divide a numeric vector into bins
#'
#' @param x A numeric vector
#' @param width The width for each bin
#' @param boundary A boundary on which to change bin
#' @param scale Currently unused
#' @param min,max Values which fall outside of these ranges will be grouped 
#'   together
#' @param format Can be either:
#'   *  A vectorised function taking two inputs (the lower and upper bounds of the 
#'      bin) and returning a character vector
#'   *  A glue-string using the special values `x` and `y`, e.g. `"[{x}, {y})"`
#' @param format_smaller,format_bigger Can be used to provide special formatting
#'   for number smaller/bigger than min/max. 
#' @param equality_side If `NULL`:
#'   *  Intervals will be closed on the left side if `format` is a string 
#'      beginning with `[` 
#'   *  Intervals will be closed on the right side 
#'   Otherwise, specify `"left"`/`"right"` to for intervals closed on the 
#'   left/right
#' @param reverse If `TRUE`, factors will be ordered largest to smallest
#'
#' @return A factor
#' @export
#'
#' @examples
#' comma <- function(x) format(x, big.mark = ",", scientific = FALSE, justify = "none")
#' 
#' bin(
#'   c(-100000, -3300, 1, 10, 40, 9030), 5000, 
#'   format = "{comma(x)} to {comma(y)}", 
#'   format_smaller = "Less than {comma(y)}",
#'   format_bigger = "Bigger than {comma(x)}",
#'   min = -20, max = 4000
#' )
bin <- function(x, width, boundary = 0,
                scale = NULL,
                min = -Inf, max = Inf, 
                format = NULL, 
                format_smaller = format,
                format_bigger = format,
                equality_side = NULL,
                reverse = FALSE) {
  
  scale <- if (is.null(scale)) identity else scale
  
  stopifnot(is.numeric(x), is.function(scale))
  
  format <- list(smaller = format_smaller, middle = format, bigger = format_bigger) |>
    lapply(function(f) {
      if (is.null(f)) {
        function(x, y) {
          x <- format(x, scientific = FALSE, big.mark = ",", trim = TRUE)
          y <- format(y, scientific = FALSE, big.mark = ",", trim = TRUE)
          glue::glue(f, x = x, y = y)
        }
      } else if (is.character(f)) {
        function(x, y) {
          glue::glue(f, x = x, y = y)
        }
      } else {
        rlang::as_function(f)
      }
    })
  
  equality_side <- if (!is.null(equality_side)) {
    match.arg(equality_side, c("left", "right"))
  } else if (grepl("\\]$", format$middle(1, 2))) {
    "right"
  } else {
    "left"
  }
  
  equality_side_correction <- if (equality_side == "right") width else 0
  lower_bound = x - (x - boundary) %% width - equality_side_correction
  upper_bound = lower_bound + width
  
  smaller <- x < min
  middle  <- min <= x & x <= max
  bigger  <- max < x 
  
  original <- x
  
  x[smaller] <- format$smaller(-Inf, min)
  x[middle]  <- format$middle(lower_bound[middle], upper_bound[middle])
  x[bigger]  <- format$bigger(max, Inf)
  
  new_levels <- names(sort(tapply(original, list(x), base::min), decreasing = reverse))
  factor(x, new_levels, ordered = FALSE)
  
}
