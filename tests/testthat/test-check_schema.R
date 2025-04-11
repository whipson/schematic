test_that("check_schema works for valid schema", {
  df <- mtcars
  sch <- schema(
    c(am, hp, cyl) ~ is.numeric
  )
  expect_invisible(check_schema(df, sch))
})

test_that("check_schema fails for invalid columns", {
  df <- mtcars
  sch <- schema(
    c(cyl, am) ~ is.character
  )
  expect_error(check_schema(df, sch), "Schema Error")
})

test_that("check_schema works on multiple entry schema", {
  df <- iris
  sch <- schema(
    c(Sepal.Length, Sepal.Width) ~ is.numeric,
    Species ~ is.factor
  )
  expect_invisible(check_schema(df, sch))
})

test_that("check_schema works using tidyselect", {
  df <- iris
  sch <- schema(
    tidyselect::starts_with("Sepal") ~ is.numeric
  )
  expect_invisible(check_schema(df, sch))
})

test_that("check_schema errors on missing cols", {
  df <- data.frame(a = 1:10, b = letters[1:10], d = 1L:10L)
  sch <- schema(
    a ~ is.numeric,
    c(c, d) ~ is.integer
  )
  expect_error(check_schema(df, sch), regexp = "missing from")
})

test_that("check_schema works with purrr-style anonymous functions", {
  df <- iris
  sch <- schema(
    Species ~ ~(is.character(.x) | is.factor(.x))
  )
  expect_invisible(check_schema(df, sch))
})

test_that("check_schema works with named args", {
  my_schema <- schema(
    `is a positive whole number` = pos_int ~ is_positive_integer
  )

  my_df <- data.frame(
    pos_int = c(-1, 2, 0.4)
  )

  expect_error(
    check_schema(my_df, my_schema),
    regexp = "is a positive whole number"
  )
})

test_that("check_schema errors on non TRUE/FALSE check", {
  df <- mtcars
  sch <- schema(
    c(cyl, am) ~ as.character
  )
  expect_error(
    expect_warning(check_schema(df, sch), "All predicate")
  )
})

test_that("check_schema works on non-definite selector", {
  df <- mtcars
  sch <- schema(
    starts_with("blahblah") ~ as.character
  )
  expect_invisible(check_schema(df, sch))
})

test_that("rules with an error are handled", {
  sch <- schema(
    cyl ~ stop()
  )

  expect_error(
    check_schema(
      mtcars,
      sch
    ),
    regexp = "Error in predicate"
  )

})
