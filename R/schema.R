#' Create a schema object
#'
#' @param ... Formulae of the form tidyselect_expr ~ predicate
#' @return A Schema object
#' @importFrom tidyselect starts_with ends_with contains matches num_range all_of any_of everything last_col where
#' @export
#' @examples
#' # Simple schema with one declared column
#' my_schema <- schema(
#'   mpg ~ is.double
#' )
#'
#' # Multiple columns
#' my_schema <- schema(
#'   Sepal.Length ~ is.numeric,
#'   Species ~ is.factor
#' )
#'
#' # Use tidyselect syntax and anonymous functions
#' my_schema <- schema(
#'   starts_with("Sepal") ~ is.numeric,
#'   c(Petal.Length, Petal.Width) ~ function(x) all(x > 0)
#' )
#'
#' # Use named arguments to customize error messages
#' my_schema <- schema(
#'   `Must be a positive number` = cyl ~ function(x) all(x > 0)
#' )
#'
schema <- function(...) {

  dots <- rlang::enquos(...)

  withCallingHandlers(
    rules <- tryCatch({
      purrr::map(dots, rlang::eval_tidy)
    }, error = function(e) {
      cli::cli_abort(
        c("Each argument to `schema()` must be a formula (see examples below).",
          "i" = "`col ~ is.numeric`",
          "i" = "`c(col1, col2) ~ is.character`"
        ),
        call = rlang::caller_env(n = 4)
      )
    }),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )

  # validate each element is a formula: lhs ~ rhs
  withCallingHandlers(
    validated <- purrr::map(rules, function(x) {
      if (!rlang::is_formula(x, lhs = TRUE)) {
        cli::cli_abort(
          c("Each argument to `schema()` must be a formula (see examples below).",
            "i" = "`col ~ is.numeric`",
            "i" = "`c(col1, col2) ~ is.character`"
          ),
          call = rlang::caller_env(n = 3)
        )
      }
      x
    }),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )


  structure(validated, class = "Schema")
}

#' Print method for Schema
#' @param x Object of class Schema
#' @param ... Other arguments passed to `print()`
#' @exportS3Method
#' @return invisible
print.Schema <- function(x, ...) {
  cli::cli_h1("Schema")
  purrr::walk(x, ~ {
    selector <- rlang::as_label(rlang::f_lhs(.x))
    predicate <- rlang::as_label(rlang::f_rhs(.x))
    cli::cli_text("{.strong {selector}} ~ {.code {predicate}}")
  })
  invisible(x)
}
