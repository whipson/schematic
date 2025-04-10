#' Create a schema object
#'
#' @param ... Formulae of the form tidyselect_expr ~ predicate
#' @return A Schema object
#' @importFrom tidyselect starts_with ends_with contains matches num_range all_of any_of everything last_col where
#' @export
schema <- function(...) {

  dots <- rlang::enquos(...)
  rules <- purrr::map(dots, rlang::eval_tidy)

  # validate each element is a formula: lhs ~ rhs
  withCallingHandlers(
    validated <- purrr::map(rules, function(x) {
      if (!rlang::is_formula(x, lhs = TRUE)) {
        cli::cli_abort(
          c("Each argument to `schema()` must be a formula (see examples below).",
            "i" = "`col ~ is.numeric`",
            "i" = "`c(col1, col2) ~ is.character`"
          ),
          call = rlang::caller_env(n = 3))
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
print.Schema <- function(x, ...) {
  cli::cli_h1("Schema")
  purrr::walk(x, ~ {
    selector <- rlang::as_label(rlang::f_lhs(.x))
    predicate <- rlang::as_label(rlang::f_rhs(.x))
    cli::cli_text("{.strong {selector}} ~ {.code {predicate}}")
  })
  invisible(x)
}
