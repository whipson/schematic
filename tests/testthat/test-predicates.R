test_that("is_positive_integer", {
  expect_true(is_positive_integer(c(1:3, NA_integer_)))
  expect_false(is_positive_integer(0))
  expect_false(is_positive_integer(1.4))
  expect_false(is_positive_integer(1.0))
  expect_false(is_positive_integer(-3))
})

test_that("is_text", {
  expect_true(is_text(letters[1:4]))
  expect_true(is_text(as.factor(letters[1:4])))
  expect_false(is_text(1))
})

test_that("is_whole_number", {
  expect_true(is_whole_number(c(2.0, 4.0)))
  expect_false(is_whole_number(c(-1.4)))
})

test_that("is_incrementing", {
  expect_true(is_incrementing(1:5))
  expect_true(is_incrementing(letters[1:5]))
  expect_false(is_incrementing(c(4, 3, 0)))
  expect_false(is_incrementing(c(NA, 3, 5)))
})

test_that("is_all_distinct", {
  expect_true(is_all_distinct(c(1:5)))
  expect_false(is_all_distinct(c(1, 1, 2)))
})

test_that("is_non_null", {
  expect_true(is_non_null(1:5))
  expect_false(is_non_null(c(1, NA, 3)))
})
