test_that("can create a schema", {
  sch <- schema(
    c(am, hp, cyl) ~ is.numeric
  )
  expect_s3_class(sch, "Schema")
})

cli::test_that_cli("schema prints", {

  sch <- schema(
    c(am, hp, cyl) ~ is.numeric
  )

  expect_snapshot(
    sch
  )
})

test_that("errors on invalid schema creation", {
  expect_error(
    schema(1),
    regexp = "Each argument"
  )

  expect_error(
    schema(
      cyl
    ),
    regexp = "Each argument"
  )
})
