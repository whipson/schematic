#' Check if a vector has all positive integers
#'
#' A positive integer is a whole number that is greater than 0.
#'
#' This check requires `is.integer(x)` to be true. If you want a more
#' flexible check that allows for numbers of type `numeric` but still
#' want them to be integers, then use `is_whole_number()`.
#'
#' `NA`s are ignored as long as they are `NA_integer`.
#'
#' @param x A vector
#' @return TRUE if all elements are positive integers (NA ignored)
#' @export
#' @examples
#' is_positive_integer(c(1L, 2L, 4L)) # TRUE
#' is_positive_integer(2.4) # FALSE
#' is_positive_integer(-3) # FALSE
is_positive_integer <- function(x) {
  is.integer(x) && all(x > 0, na.rm = TRUE)
}

#' Check if a vector is text-based (character or factor)
#'
#' `NA`s are ignored as long as they are `NA_character_`.
#'
#' @param x A vector
#' @return TRUE if vector is either character or factor
#' @export
#' @examples
#' is_text(letters[1:4]) # TRUE
#' is_text(as.factor(letters[1:4])) # TRUE
#' is_text(1) # FALSE
is_text <- function(x) {
  is.character(x) | is.factor(x)
}

#' Check if a vector has all whole numbers
#'
#' Similar to `is_positive_integer()` but without the constraint that the
#' underlying data type is actually integer. Useful if the numbers are stored
#' as `numeric` but you want to check that they are whole.
#'
#' `NA`s are ignored.
#'
#' @param x A vector
#' @return TRUE if all elements are whole numbers (NA ignored)
#' @export
#' @examples
#' is_whole_number(c(2.0, 4.0)) # TRUE
#' is_whole_number(c(-1.4)) # FALSE
is_whole_number <- function(x) {
  all((x %% 1) == 0, na.rm = TRUE)
}

#' Check if the vector is sorted numerically or alphanumerically
#'
#' `NA`s are not ignored and any vector with `NA`s will fail unless the whole vector is `NA`.
#'
#' @param x A vector
#'
#' @returns TRUE if the vector is sorted
#' @export
#' @examples
#' is_incrementing(1:5) # TRUE
#' is_incrementing(letters[1:5]) # TRUE
#' is_incrementing(c(4, 3, 0)) # FALSE
is_incrementing <- function(x) {
  identical(x, sort(x))
}

#' Check if all values in a vector are distinct
#'
#' @param x A vector
#'
#' @returns TRUE if the vector has all unique values
#' @export
#'
#' @examples
#' is_all_distinct(c(1:5)) # TRUE
#' is_all_distinct(c(1, 1, 2)) # FALSE
is_all_distinct <- function(x) {
  length(unique(x)) == length(x)
}


#' Check if all values are not NA
#'
#' @param x A vector
#'
#' @returns TRUE if the vector has no NA values
#' @export
#'
#' @examples
#' is_non_null(1:5) # TRUE
#' is_non_null(c(1, NA, 3)) # FALSE
is_non_null <- function(x) {
  all(!is.na(x))
}
