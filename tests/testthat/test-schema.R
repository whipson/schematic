test_that("can create a schema", {
  sch <- schema(
    c(am, hp, cyl) ~ is.numeric
  )
  expect_s3_class(sch, "Schema")
})

test_that("errors on invalid schema creation", {
  expect_error(
    schema(1),
    regexp = "Each argument"
  )
})
