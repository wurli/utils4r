#' Display editable data
#'
#' This is a wrapper for [rhandsontable::rhandsontable()] which provides a
#' easier/more user-friendly interface for setting column/cell settings. NB,
#' some of the features enabled by this function aren't possible with the
#' rhandsontable API - in particular, setting options for a single cell. These
#' features are enabled by [with_hot_settings()] - this function should be used
#' to set options for individual columns before calling `pretty_hot()`.
#'
#' @param x A data frame
#' @param default_colwidth The default column width
#' @param ... Passed to [rhandsontable::rhandsontable()]
#'
#' @return A handsontable object
#'
#' @seealso [with_hot_settings()] to apply settings to rows/columns
#'
#' @examples
#' 
#' library(dplyr)
#' 
#' my_data <- tibble(
#'   text = c("foo", "bar", "baz"),
#'   nums = c("100", "200", "12/12/2020"),
#'   tooltip = c("This is the first cell", NA, NA)
#' )
#' 
#' my_data %>%
#'   mutate(
#'     text = with_hot_settings(
#'       text,
#'       comment = tooltip,
#'       possible_values = case_when(row_number() == 3 ~ list(c("baz", "bazzy"))),
#'       col_width = 300,
#'       row_heights = ifelse(row_number() == 3, 500, 40)
#'     ),
#'     nums = with_hot_settings(
#'       nums,
#'       is_date = c(FALSE, FALSE, TRUE)
#'     ),
#'     .keep = "none"
#'   ) %>%
#'   pretty_hot()
pretty_hot <- function(x, default_colwidth = 200, ...) {

  cell_settings <- x %>%
    as.list() %>%
    unname() %>%
    imap(function(col, i) {
      if (!has_hot_settings(col)) return(NULL)

      col %>%
        unname() %>%
        imap(function(cell, j) {

          # Row/col, read-only status
          out <- list(
            row      = j - 1L,
            col      = i - 1L,
            readOnly = cell$read_only
          )

          # Comments for the cell
          comment <- cell$comment
          if (!is.null(comment) && all(!is.na(comment)) && all(comment != "")) {
            out$comment <- list(
              value = comment,
              style = list(
                width = cell$comment_width %||% 350,
                height = cell$comment_height %||% 200
              )
            )
          }

          # Date format
          if (isTRUE(cell$is_date)) {
            out$type          <- "date"
            out$correctFormat <- FALSE
            out$dateFormat    <- "DD/MM/YYYY"
            # out$allowInvalid <- FALSE # nolint: commented_code_linter
          }

          # Enumeration format
          if (!is.null(cell$possible_values)) {
            out$type   <- "dropdown"
            out$source <- cell$possible_values
          }

          # Alignment
          out$className <- paste(c(cell$align_v, cell$align_h), collapse = " ")

          # Cell merging
          if (!is.null(cell$merge)) {
            out$merge <- c(
              out[c("row", "col")],
              list(rowspan = cell$merge$rowspan, colspan = 1)
            )
            out$className <- paste(c(out$className, "htMiddle"), collapse = " ")
          }

          out
        })
    }) %>%
    purrr::list_flatten() %>%
    purrr::compact()

  hot_data <- x %>%
    mutate(
      across(where(has_hot_settings), function(col) {
        map_chr(col, "text")
      })
    )

  col_widths <- x %>%
    map_dbl(function(col) {
      out <- if (has_hot_settings(col)) attr(col, "hot_colwidth") else default_colwidth
      out %||% 100
    }) %>%
    unname()

  row_heights <- coalesce(
    !!!map(keep(x, has_hot_settings), attr, "hot_rowheights"),
    rep(30, nrow(x))
  )

  merge <- map(cell_settings, "merge") %>%
    purrr::compact()

  out <- rhandsontable::rhandsontable(
    hot_data,
    ..., 
    comments = data.frame(),
    rowHeaders = FALSE,
    cell = cell_settings,
    colWidths = col_widths,
    mergeCells = merge,
    allowedTags = "<em><b><strong><a><big><span><ul><li>",
  )

  out$x$rowHeights  <- row_heights
  out$x$contextMenu <- list()

  for (i in seq_len(ncol(x))) {
    out$x$columns[[i]]$renderer <- if (has_hot_settings(x[[i]])) {
      htmlwidgets::JS(attr(x[[i]], "hot_renderer"))
    } else {
      htmlwidgets::JS(accessibility_renderer())
    }
  }

  out

}

#' Set options for table columns/cells
#'
#' @param x A vector (i.e. a data frame column)
#' @param comment Text for the cell's comment. If a single value, this will
#'   be used for the whole column.
#' @param comment_width,comment_height Dimensions for the comment box
#' @param possible_values A list of character vectors, where each vector
#'   gives the valid options for the cell
#' @param read_only A logical vector. If a single value, this value will be 
#'   used for the whole column.
#' @param is_date A logical vector. Determines whether a date selection UI
#'   should appear when the cell is selected. If a single value, this value 
#'   will be used for the whole column. 
#' @param merge_by A vector giving cells to merge by - consecutive repeating
#'   elements indicate that a merge should take place.
#' @param align_h Horizontal text alignment. Should be `NULL`, or a character
#'   vector where each value is one of `c("left", "center", "right", "justify")`
#' @param align_v Vertical text alignment. Should be `NULL`, or a character
#'   vector where each value is one of `c("top", "middle", "bottom")`
#' @param col_width The width for the column
#' @param row_heights A numeric vector giving heights for individual rows.
#'   This will obviously affect the other columns in the table too.
#' @param col_renderer A character string giving a javascript renderer to use
#'   for the column.
#'
#' @return A list
#'
with_hot_settings <- function(x,
                              comment = NULL,
                              comment_width = NULL,
                              comment_height = NULL,
                              possible_values = NULL,
                              read_only = FALSE,
                              is_date = FALSE,
                              merge_by = NULL,
                              align_h = NULL,
                              align_v = NULL,
                              col_width = NULL,
                              row_heights = NULL,
                              col_renderer = accessibility_renderer()) {

  if (!is.null(align_h)) {
    stopifnot(all(align_h %in% c("left", "center", "right", "justify")))
    align_h <- paste0("ht", str_to_title(align_h))
  }
  if (!is.null(align_v)) {
    stopifnot(all(align_v %in% c("top", "middle", "bottom")))
    align_v <- paste0("ht", str_to_title(align_v))
  }

  if (!is.null(merge_by)) {
    lens <- rle(merge_by)$lengths
    merge_points <- cumsum(c(1, lens[-length(lens)]))

    merge_by <- map(seq_along(x), function(row) {
      if (!row %in% merge_points) return(NULL)
      list(rowspan = lens[merge_points == row])
    })
  }

  out <- pmap(
    list(
      text = x,
      comment = comment %||% list(NULL),
      comment_width = comment_width %||% list(NULL),
      comment_height = comment_height %||% list(NULL),
      possible_values = possible_values %||% list(NULL),
      read_only = read_only,
      is_date = is_date,
      merge = merge_by %||% list(NULL),
      align_h = align_h %||% list(NULL),
      align_v = align_v %||% list(NULL)
    ),
    list
  )

  class(out) <- c("hot_settings", class(out))
  attr(out, "hot_merge")      <- merge_by
  attr(out, "hot_colwidth")   <- col_width
  attr(out, "hot_rowheights") <- row_heights
  attr(out, "hot_renderer")   <- col_renderer

  out
}

has_hot_settings <- function(x) {
  inherits(x, "hot_settings") && is.list(x)
}

accessibility_renderer <- function() {
  paste(sep = "\n",
    "function(instance, td, row, col, prop, value, cellProperties) {",
    "  Handsontable.renderers.TextRenderer.apply(this, arguments);",
    "  td.style.backgroundColor = cellProperties.readOnly ? '#F9F9F9' : 'white';",
    "  td.style.color = 'black';",
    "  td.style.fontSize = '14px';     // Change the font size to 14px",
    "  td.style.fontFamily = 'Arial';  // Change the font type to Arial",
    "  safeHtmlRenderer(instance, td, row, col, prop, value, cellProperties)",
    "}"
  )
}

