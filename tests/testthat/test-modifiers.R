test_that("mod_nullable", {
  expect_true(mod_nullable(is_all_distinct)(c(1, 3, NA, NA)))
  expect_true(mod_nullable(is_incrementing)(c(1, NA, 3)))
})

test_that("mod_infinitable", {
  x <- c(1, Inf, 3)
  is_incrementing_inf <- mod_infinitable(is_incrementing)
  expect_true(is_incrementing_inf(x))
})

test_that("mod_nullable works with custom function", {
  sch <- schema(
    c(a, b) ~ mod_nullable(~all(.x %in% c("a", "b", "c")))
  )

  expect_no_error({
    check_schema(
      data.frame(
        a = c(NA, "a", "c"),
        b = c("a", "b", "c")
      ),
      sch
    )
  })
})
