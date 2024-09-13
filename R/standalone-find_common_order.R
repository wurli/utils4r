# ---
# repo: wurli/utils4r
# file: standalone-find_common_order.R
# last-updated: 2024-09-13
# license: https://unlicense.org
# imports: [rlang, purrr] 
# ---

#' Determine a common ordering from a set of vectors
#' 
#' @param ... Input vectors
#' @param .strict If `TRUE`, an error will be thrown if no common ordering 
#'   exists. If `FALSE`, a warning will be thrown and the output will be
#'   returned. Note that to use the output in such cases, you may want to 
#'   call `unique()` to remove any duplicates.
#' 
#' @examples
#' find_common_order(
#'   c(1, 3, 4, 7, 8),
#'   c(2, 3, 4, 8, 9),
#'   c(4, 5, 6, 7, 9)
#' )
#'
#' find_common_order(
#'   c("bananas", "oranges", "apples"),
#'   c("oranges", "avocados", "apples"),
#'   c("apples", "mangos", "lemons")
#' )
#'
#' find_common_order(
#'   c("foo", "bar"),
#'   c("bar", "baz"),
#'   c("baz", "foo")
#' )
find_common_order <- function(..., .strict = TRUE) {
  # RLE not strictly necessary, but potentially improves performance
  vecs <- vec_original <- list2(...) |> map(rle) |> map("values")
  out  <- NULL

  while (length(vecs) > 0L) {
    # Get the first element from each vector
    first_elements <- vecs |>
      map_vec(1L, .ptype = vecs[[1L]]) |>
      unique()

    # Get the maximum position of each candidate element across all vectors
    max_positions <- first_elements |> 
      map_int(function(e) max(map_int(vecs, ~ max(which(. == e) %or% 1L))))

    # Choose the one with the lowest maximum position as the next in order
    next_element <- first_elements[max_positions == min(max_positions)][1]
    out          <- c(out, next_element)
    # Remove the next element from the start of each vector
    vecs         <- vecs |> map_if(~ .[1L] == next_element, ~ .[-1L]) |> compact()
  }

  # If any element appears in the output twice then no universal ordering exists
  dupes <- tapply(out, out, length, simplify = FALSE) |> keep(~ . > 1L) |> names() |> unique()

  if (length(dupes) == 0L) {
    return(out)
  }

  # Error if no universal ordering exists
  names(vec_original) <- if (!is.null(...names())) {
    map_chr(...names(), function(x) cli::format_inline("{.arg {x}}"))
  } else {
    paste("Argument", seq_len(...length()))
  }
  dupe_positions <- which(out == dupes[1])
  bad_elements   <- out[seq(dupe_positions[1L], dupe_positions[2L])]
  info           <- map2_chr(
    bad_elements[-length(bad_elements)], bad_elements[-1L],
    function(x, y) {
      vec_name <- vec_original |> 
        map(intersect, c(x, y)) |> 
        keep(identical, c(x, y)) |> 
        names() |> 
        _[1]

      cli::format_inline("{vec_name}: {.val {x}} comes before {.val {y}}")
    }
  )
  
  throw <- if (.strict) cli::cli_abort else cli::cli_warn

  throw(c(
    "No universal ordering exists",
    set_names(info, c(rep("v", length(info) - 1L), "x")),
    if (length(dupes) > 1L) c(
      i = "And {length(dupes) - 1L} other similar cases"
    )
  ))
  
  out
}

`%or%` <- function(x, y) if (length(x) == 0L) y else x
