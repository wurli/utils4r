#' Invoke a Data Viewer
#' 
#' An altered version of `View()` that works with memoised functions.
#' See [this StackOverflow question](https://stackoverflow.com/questions/59721210/is-there-any-way-to-change-the-way-view-works-for-different-classes`)
#' for details.
#' 
#' @param x,title Passed to `utils::View()`
#' @noRd
#' @export
View <- function(x, title) {
  UseMethod("View")
}

#' @export
View.default <- function(x, title) {
  eval(substitute(
    get("View", envir = as.environment("package:utils"))(x,title)
  ))
}

#' @export
View.memoised <- function(x, title) {
  eval(bquote(View(.(as.list(environment(x))$`_f`), title)))
}
