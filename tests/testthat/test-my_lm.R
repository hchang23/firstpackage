# within test-my_lm.R
test <- my_lm(mpg ~ hp, mtcars)

test_that("my_lm outputs a table", {
  expect_is(my_lm(mpg ~ hp + wt, mtcars), "table")
})

test_that("non-table input throws an error", {
  expect_error(my_lm("a string"))
})

test_that("estimate for each coefficient is calculated correctly", {
  expect_equal(test[1, 1], 30.09886, tolerance = 0.0001)
  expect_equal(test[2, 1], -0.06823, tolerance = 0.0001)
})

test_that("standard error for each coefficient is calculated correctly", {
  expect_equal(test[1, 2], 1.63392, tolerance = 0.0001)
  expect_equal(test[2, 2], 0.01012, tolerance = 0.0001)
})

test_that("t value for each coefficient is calculated correctly", {
  expect_equal(test[1, 3], 18.421, tolerance = 0.0001)
  expect_equal(test[2, 3], -6.742, tolerance = 0.0001)
})

test_that("p value for each coefficient is calculated correctly", {
  expect_equal(test[1, 4], 6.6427e-18, tolerance = 0.0001)
  expect_equal(test[2, 4], 1.79e-07, tolerance = 0.0001)
})
