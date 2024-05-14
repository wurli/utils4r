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
    rep(10, nrow(x))
  )

  out <- rhandsontable::rhandsontable(
    hot_data,
    ..., 
    comments = data.frame(),
    rowHeaders = FALSE,
    cell = cell_settings,
    colWidths = col_widths,
    allowedTags = "<em><b><strong><a><big><span><ul><li>"
  )

  out$x$rowHeights  <- row_heights
  out$x$contextMenu <- list()

  for (i in seq_len(ncol(x))) {
    if (!has_hot_settings(x[[i]])) next
    out$x$columns[[i]]$renderer <- htmlwidgets::JS(attr(x[[i]], "hot_renderer"))
  }

  out

}

with_hot_settings <- function(x,
                              comment = NULL,
                              comment_width = NULL,
                              comment_height = NULL,
                              possible_values = NULL,
                              read_only = FALSE,
                              is_date = FALSE,
                              col_width = NULL,
                              row_heights = NULL,
                              col_renderer = accessibility_renderer()) {
  out <- pmap(
    list(
      text = x,
      comment = comment %||% list(NULL),
      comment_width = comment_width %||% list(NULL),
      comment_height = comment_height %||% list(NULL),
      possible_values = possible_values %||% list(NULL),
      read_only = read_only,
      is_date = is_date
    ),
    list
  )

  class(out) <- c("hot_settings", class(out))
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
    "  td.style.color = 'black';       // Change the text color to black",
    "  td.style.fontSize = '14px';     // Change the font size to 14px",
    "  td.style.fontFamily = 'Arial';  // Change the font type to Arial",
    "  safeHtmlRenderer(instance, td, row, col, prop, value, cellProperties)",
    "}"
  )
}