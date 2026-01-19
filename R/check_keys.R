check_keys <- function(x, ...) {
	count_colname <- ".count"

	if (count_colname %in% colnames(x)) {
		cli::cli_abort(
			"Dataframe to check must not have a column named {.field {count_colname}}"
		)
	}

	counts <- if (...length() == 0) {
		count(x, pick(everything()), name = count_colname)
	} else {
		count(x, ..., name = count_colname)
	}

	n_bad_rows <- counts |>
		filter(.data[[count_colname]] > 1) |>
		pull(.data[[count_colname]]) |>
		sum()

	if (n_bad_rows > 0) {
		if (...length() == 0) {
			cli::cli_abort(c(
				"Rows are not uniquely identified",
				i = "{.val {n_bad_rows}} non-unique rows detected"
			))
		} else {
			selected_cols <- colnames(select(x, ...))
			cli::cli_abort(c(
				"Rows are not uniquely identified by {.field {selected_cols}}",
				i = "{.val {n_bad_rows}} non-unique rows detected"
			))
		}
	}

	invisible(x)
}

