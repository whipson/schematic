#' Allow NA in a predicate
#'
#' This modifies a predicate function to ignore NAs.
#'
#' @param pred A predicate function
#' @return A new predicate that allows NAs
#' @export
#' @examples
#' # The `is_incrementing` predicate will fail if there are NAs
#' x <- c(1, NA, 3)
#' is_incrementing(x) # FALSE
#'
#' is_incrementing_null <- mod_nullable(is_incrementing)
#' is_incrementing_null(x) # TRUE
mod_nullable <- function(pred) {
  pred <- rlang::as_function(pred)
  function(x) {
    if (all(is.na(x))) return(TRUE)
    pred(x[!is.na(x)])
  }
}

#' Ignore infinite values in a predicate
#'
#' This modifies a predicate function to ignore Inf.
#'
#' @param pred A predicate function
#' @return A new predicate that ignores infinites
#' @export
#' @examples
#' # The `is_incrementing` predicate will fail here
#' x <- c(1, Inf, 3)
#' is_incrementing(x) # FALSE
#'
#' is_incrementing_inf <- mod_infinitable(is_incrementing)
#' is_incrementing_inf(x) # TRUE
mod_infinitable <- function(pred) {
  pred <- rlang::as_function(pred)
  function(x) {
    if (all(is.na(x))) return(TRUE)
    pred(x[!is.infinite(x)])
  }
}
