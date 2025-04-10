#' Validate a data.frame against a schema
#'
#' @param data A data.frame to check
#' @param schema A Schema object created with `schema()`
#' @return invisible if validation passes, otherwise stops with error
#' @export
check_schema <- function(data, schema) {

  stopifnot(inherits(schema, "Schema"))

  pred_names <- names(schema)
  pred_names <- ifelse(trimws(pred_names) == "", NA, pred_names)

  missing_cols_vec <- c()

  results <- purrr::map(schema, ~{
    selector <- rlang::f_lhs(.x)
    predicate_fm <- rlang::f_rhs(.x)
    predicate <- predicate_fm |>
      rlang::eval_tidy() |>
      rlang::as_function()

    cols <- tryCatch({
      tidyselect::eval_select(selector, data) |>
        names()
    }, error = function(e) {
      still_exists <- tidyselect::eval_select(selector, data, strict = FALSE) |>
        names()
      check_cols <- clean_c_wrapper(
        rlang::as_label(selector)
      ) |>
        strsplit(",\\s*")
      check_cols <- check_cols[[1]]
      missing_cols_vec <<- c(missing_cols_vec, setdiff(check_cols, still_exists))
      still_exists
    })

    purrr::map(cols, ~{
      actual_class <- class(data[[.x]])
      result <- tryCatch({
        stopifnot(predicate(data[[.x]]))
        TRUE
      }, error = function(e) {
        FALSE
      })
      if (isTRUE(result)) return(NULL)
      data.frame(
        col = .x,
        class = actual_class,
        predicate = rlang::as_label(predicate_fm)
      )
    }) |>
      purrr::list_rbind()
  })

  fail_idx <- which(purrr::map_lgl(results, ~nrow(.x) > 0))
  pred_names <- pred_names[fail_idx]

  if (length(fail_idx) == 0 && length(missing_cols_vec) == 0) return(invisible())

  out_c <- c()

  if (!is.null(missing_cols_vec)) {
    purrr::walk(missing_cols_vec, ~{
      cols_ticked <- paste0("`", .x, "`")
      msg <- glue::glue(cli::pluralize("Column{?s} {cols_ticked} missing from data"))
      out_c <<- c(out_c, msg)
    })
  }

  purrr::walk2(results[fail_idx], pred_names, ~{
    cols <- .x$col
    pred <- if (is.na(.y)) .x$predicate else .y

    cols_ticked <- paste0("`", cols, "`")
    plural_prefix <- cli::pluralize("Column{?s} {cols_ticked}")
    msg <- glue::glue("{plural_prefix} failed check `{pred}`")
    out_c <<- c(out_c, msg)
  })

  cli::cli_abort(c("Schematic Error:\n", paste("-", out_c)))

  invisible()
}
