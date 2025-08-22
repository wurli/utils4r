# ---
# repo: wurli/utils4r
# file: standalone-df_compare.R
# last-updated: 2025-08-22
# license: https://unlicense.org
# imports: [purrr, dplyr, tidyr, cli, glue]
# ---

#' Compare two data frames
#'
#'
#' @param df1,df2 Data frames to compare. Should have some columns in common.
#' @param key_cols Character vector of column names to use as keys for joining
#' @return A data frame listing differences between `df1` and `df2`.
df_compare <- function(df1, df2, key_cols, names = c("Old", "New")) {
  common_cols <- intersect(colnames(df1), colnames(df2))
  value_cols <- setdiff(common_cols, key_cols)

  discarded_x <- setdiff(colnames(df1), common_cols)
  discarded_y <- setdiff(colnames(df2), common_cols)

  if (length(discarded_x) > 0) {
    cli::cli_alert_warning("Ignoring cols from old table: {.field {discarded_x}}")
  }
  if (length(discarded_y) > 0) {
    cli::cli_alert_warning("Ignoring cols from new table: {.field {discarded_y}}")
  }

  unjoined_rows_x <- df1 |> anti_join(df2, by = key_cols) |> nrow()
  unjoined_rows_y <- df2 |> anti_join(df1, by = key_cols) |> nrow()

  if (unjoined_rows_x > 0) {
    cli::cli_alert_warning("Could not join {.val {unjoined_rows_x}} rows from old table")
  }
  if (unjoined_rows_y > 0) {
    cli::cli_alert_warning("Could not join {.val {unjoined_rows_y}} rows from new table")
  }

  x_name <- names[1]
  y_name <- names[2]

  joined <- dplyr::inner_join(
    df1,
    df2,
    by = key_cols,
    relationship = "one-to-one"
  ) |>
    dplyr::select(dplyr::any_of(c(
      common_cols,
      paste0(common_cols, ".x"),
      paste0(common_cols, ".y")
    )))

  if (nrow(joined) == 0) {
    cli::cli_alert_warning("No joined rows to compare")
    return(invisible(joined))
  } else {
    cli::cli_alert_success("Succesfully joined {.val {nrow(joined)}} rows to compare")
  }

  out <- joined |>
    dplyr::mutate(
      value_cols |>
        purrr::set_names() |>
        purrr::imap(function(col, col_name) {
          x <- get(paste0(col, ".x"))
          y <- get(paste0(col, ".y"))

          if (is.character(x) || is.character(y)) {
            x <- as.character(x)
            y <- as.character(y)
          }

          if (is.numeric(x) || is.numeric(y)) {
            x <- as.numeric(x)
            y <- as.numeric(y)
          }

          if (is.numeric(x)) {
            compare <- function(x, y) abs(x - y) < 0.000001
          } else {
            compare <- `==`
          }

          dplyr::tibble(
            "{col_name}_Match" := dplyr::if_else(
              dplyr::coalesce((is.na(x) & is.na(y)) | compare(x, y), FALSE),
              "Match",
              "NoMatch"
            ),
            "{col_name}_{x_name}" := as.character(x),
            "{col_name}_{y_name}" := as.character(y)
          )
        }) |>
        dplyr::bind_cols()
    ) |>
    dplyr::select(-dplyr::ends_with(c(".x", ".y"))) |>
    tidyr::pivot_longer(
      -key_cols,
      names_pattern = glue::glue("^(.+)_({x_name}|{y_name}|Match)$"),
      names_to = c("Field", ".value")
    ) |>
    dplyr::filter(Match == "NoMatch")

  if (nrow(out) == 0) {
    cols <- cli::cli_vec(value_cols, list("vec-trunc" = Inf))
    cli::cli_alert_success("No differences detected in any of {.field {cols}}")
    return(invisible(out))
  }

  out
}

