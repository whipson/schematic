#' Validate a data.frame against a schema
#'
#' @param data A data.frame to check
#' @param schema A Schema object created with `schema()`
#' @return invisible if validation passes, otherwise stops with error
#' @export
#' @examples
#'
#' my_schema <- schema(
#'   mpg ~ is.numeric
#' )
#'
#' check_schema(mtcars, my_schema)
#'
check_schema <- function(data, schema) {

  stopifnot(inherits(schema, "Schema"))
  stopifnot(inherits(data, "data.frame"))

  pred_names <- names(schema)
  pred_names <- ifelse(trimws(pred_names) == "", NA, pred_names)

  missing_cols_vec <- c()
  invalid_preds_list <- list()
  errored_preds_list <- list()
  check_errors <- list()

  withCallingHandlers(
    results <- purrr::map(schema, ~{

      selector <- rlang::f_lhs(.x)
      predicate_fm <- rlang::f_rhs(.x)
      predicate_label <- rlang::as_label(predicate_fm)

      # Initialize some containers
      invalid_preds_list <<- append(
        invalid_preds_list,
        list(character()) |>
          purrr::set_names(predicate_label)
      )

      errored_preds_list <<- append(
        errored_preds_list,
        list(character()) |>
          purrr::set_names(predicate_label)
      )

      tryCatch({
        predicate <- predicate_fm |>
          rlang::eval_tidy() |>
          rlang::as_function()
      }, error = function(e) {
        cli::cli_abort(
          glue::glue("Error in predicate `{predicate_label}`"),
          call = rlang::caller_env(n = 7)
        )
      })

      cols <- tryCatch({
        tidyselect::eval_select(selector, data) |>
          names()
      }, error = function(`___e`) { # avoid conflicts with columns named 'e'
        still_exists <- tidyselect::eval_select(selector, data, strict = FALSE) |>
          names()
        check_cols <- clean_c_wrapper(
          paste(deparse(selector), collapse = "")
        ) |>
          strsplit(",\\s*")
        check_cols <- check_cols[[1]]
        missing_cols_vec <<- c(missing_cols_vec, setdiff(check_cols, still_exists))
        still_exists
      })

      purrr::map(cols, ~{
        actual_class <- class(data[[.x]])
        result <- tryCatch({
          check_pass <- tryCatch({
            predicate(data[[.x]])
          }, error = function(e) {
            errored_preds_list[[predicate_label]] <<- c(errored_preds_list[[predicate_label]], .x)
            check_errors <<- append(check_errors, purrr::set_names(e$message, predicate_label))
            FALSE
          })
          if (!is.logical(check_pass)) {
            invalid_preds_list[[predicate_label]] <<- c(invalid_preds_list[[predicate_label]], .x)
            FALSE
          }
          stopifnot(check_pass)
          TRUE
        }, error = function(e) {
          FALSE
        })
        if (isTRUE(result)) return(NULL)
        data.frame(
          col = .x,
          class = actual_class,
          predicate = predicate_label
        )
      }) |>
        purrr::list_rbind()
    }),

    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )

  schematic_pkgenv$check_errors <- check_errors

  fail_idx <- which(purrr::map_lgl(results, ~nrow(.x) > 0))
  pred_names <- pred_names[fail_idx]

  if (length(fail_idx) == 0 && length(missing_cols_vec) == 0) return(invisible())

  out_c <- c()

  # Warn on errored predicates
  errored_preds_list <- purrr::discard(errored_preds_list, ~length(.x) == 0)
  if (length(errored_preds_list) > 0) {
    out_err <- c()
    purrr::iwalk(errored_preds_list, ~{
      cols_ticked <- paste0("`", .x, "`")
      plural_prefix <- glue::glue(cli::pluralize("column{?s} {cols_ticked}"))
      msg <- glue::glue("`{.y}` on {plural_prefix}")
      out_err <<- c(out_err, msg)
    })
    plural_pred <- ""
    if (length(out_err) > 1) {
      plural_pred <- "s"
    }
    cli::cli_warn(
      c(
        glue::glue("Error in predicate{plural_pred}:\n"),
        paste("-", out_err),
        "i" = "Run {.fn schematic::last_check_errors} to see where the error{plural_pred} occurred."
      )
    )
  }

  # Warn on invalid predicates (non TRUE/FALSE)
  invalid_preds_list <- purrr::discard(invalid_preds_list, ~length(.x) == 0)
  if (length(invalid_preds_list) > 0) {
    out_inv <- c()
    purrr::iwalk(invalid_preds_list, ~{
      cols_ticked <- paste0("`", .x, "`")
      plural_prefix <- glue::glue(cli::pluralize("column{?s} {cols_ticked}"))
      msg <- glue::glue("`{.y}` on {plural_prefix}")
      out_inv <<- c(out_inv, msg)
    })
    plural_pred <- ""
    if (length(out_inv) > 1) {
      plural_pred <- "s"
    }
    cli::cli_warn(
      c(
        glue::glue("Invalid predicate{plural_pred}:\n"), paste("-", out_inv),
        "i" = "All predicate functions must return a single TRUE/FALSE"
      )
    )
  }

  if (!is.null(missing_cols_vec)) {
    cols_ticked <- paste0("`", missing_cols_vec, "`")
    msg <- glue::glue(cli::pluralize("Column{?s} {cols_ticked} missing from data"))
    out_c <- c(out_c, msg)
  }

  purrr::walk2(results[fail_idx], pred_names, ~{
    cols <- .x$col
    pred <- if (is.na(.y)) .x$predicate[[1]] else .y

    cols_ticked <- paste0("`", cols, "`")
    plural_prefix <- cli::pluralize("Column{?s} {cols_ticked}")
    msg <- glue::glue("{plural_prefix} failed check `{pred}`")
    out_c <<- c(out_c, msg)
  })

  cli::cli_abort(c("Schema Error:\n", paste("-", out_c)), call = NULL)

  invisible()
}
