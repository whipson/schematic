#' Retrieve latest schematic run time errors
#'
#' Predicates that error will store the error messages internally and these
#' can be accessed here.
#'
#' @return error messages
#' @export
#' @examples
#'
#' last_check_errors()
last_check_errors <- function() {
  schematic_pkgenv$check_errors
}
