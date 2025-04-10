

# schematic

`schematic` helps you check a data.frame against a schema (i.e.,
data-type matching, nullability, etc.). The advantage of `schematic` is
its use of `tidyselect` syntax for declaring a schema and user-facing
messages that can be displayed in applications.

``` r
remotes::install_github("https://github.com/whipson/schematic")
```

Use `schema()` to create a Schema. Names and tidyselect expressions on
the left of the tilde (LHS) correspond to column names and expressions
on the right (RHS) are predicate functions that return TRUE or FALSE.

``` r
library(schematic)

my_schema <- schema(
  hello = ints ~ is.integer,
  pos_int ~ is_positive_integer,
  starts_with("num") ~ is.numeric,
  c(fct1, another_fct) ~ is.factor,
  times ~ function(x) inherits(x, "POSIXct")
)
```

`check_schema()` returns an informative error including the names of the
columns that failed the schema and the reason that they failed.

``` r
# Test data.frame with some errors sprinkled in
my_df <- data.frame(
  ints = 1L:5L,
  pos_int = 1L:5L,
  num1 = rnorm(5),
  num2 = rpois(1, 5),
  fct1 = letters[1:5],
  another_fct = letters[6:10],
  times = "2025-04-09"
)

check_schema(
  my_df,
  my_schema
)
```

    Error in `check_schema()`:
    ! Schematic Error:
    - Columns `fct1` and `another_fct` failed check `is.factor`
    - Columns `fct1` and `another_fct` failed check `is.factor`
    - Column `times` failed check `function(x) inherits(x, "POSIXct")`

You can also supply argument names in `schema()` to customize the output
of the message.

``` r
my_schema <- schema(
  `is a positive whole number` = pos_int ~ is_positive_integer
)

my_df <- data.frame(
  pos_int = c(-1, 2, 0.4)
)

check_schema(my_df, my_schema)
```

    Error in `check_schema()`:
    ! Schematic Error:
    - Column `pos_int` failed check `is a positive whole number`
