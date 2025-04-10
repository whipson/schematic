#' Check if a vector is a positive integer
#' @param x A vector
#' @return TRUE if all elements are positive integers (NA ignored)
#' @export
is_positive_integer <- function(x) {
  is.integer(x) && all(x > 0, na.rm = TRUE)
}

#' Allow NA in a predicate
#' @param pred A predicate function
#' @return A new predicate that allows NAs
#' @export
nullable <- function(pred) {
  function(x) {
    if (all(is.na(x))) return(TRUE)
    pred(x[!is.na(x)])
  }
}

#' Check if a vector is text-based (character or factor)
#' @param x vector
#' @return TRUE if vector is either character or factor
#' @export
is_text <- function(x) {
  is.character(x) | is.factor(x)
}

