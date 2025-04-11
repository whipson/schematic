

<!-- badges: start -->

[![R-CMD-check](https://github.com/whipson/schematic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/whipson/schematic/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/whipson/schematic/graph/badge.svg)](https://app.codecov.io/gh/whipson/schematic)
<!-- badges: end -->

# schematic

schematic makes it easy to validate a data.frame against a schema. It’s
designed to provide clear and expressive error messages that help users
understand and fix data issues.

Key features of schematic include:

✅ Declarative schema definitions using tidyselect syntax

💬 User-friendly error messages designed for use in Shiny apps and APIs

📋 Comprehensive reporting of all schema violations, not just the first

🪶 Lightweight and dependency-conscious—minimal overhead, easy to
integrate

Use schematic when you want flexible and expressive schema validation,
especially in contexts where informative feedback matters—like a Shiny
app, a plumber endpoint, or any tool that consumes user-submitted data
(e.g., .csv uploads).

``` r
remotes::install_github("https://github.com/whipson/schematic")
```

Use `schema()` to create a Schema. Names and tidyselect expressions on
the left of the tilde (LHS) correspond to column names and expressions
on the right (RHS) are predicate functions that return TRUE or FALSE.

``` r
library(schematic)

# Test data.frame with some errors sprinkled in
my_df <- data.frame(
  ints = 1L:5L,
  num1 = rnorm(5),
  num2 = rpois(1, 5),
  fct1 = letters[1:5],
  another_fct = letters[6:10],
  times = "2025-04-09"
)

my_schema <- schema(
  ints ~ is.integer,
  starts_with("num") ~ is.numeric,
  c(fct1, another_fct) ~ is.factor,
  times ~ function(x) inherits(x, "POSIXct"),
  c(some_import_col) ~ is.numeric
)
```

`check_schema()` returns an informative error including the names of the
columns that failed the schema and the reason that they failed.

``` r
check_schema(
  my_df,
  my_schema
)
```

    Error in `check_schema()`:
    ! Schema Error:
    - Column `some_import_col` missing from data
    - Columns `fct1` and `another_fct` failed check `is.factor`
    - Column `times` failed check `function(x) inherits(x, "POSIXct")`

You can also supply argument names in `schema()` to customize the output
of the message. This is particularly helpful when you want to present a
user with an informative message such as in a Shiny app.

``` r
my_schema <- schema(
  `is a whole number` = my_int ~ is.integer
)

my_df <- data.frame(
  my_int = c(-1, 2, 0.4)
)

check_schema(my_df, my_schema)
```

    Error in `check_schema()`:
    ! Schema Error:
    - Column `my_int` failed check `is a whole number`
