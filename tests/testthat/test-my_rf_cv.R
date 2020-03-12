# within test-my_rf_cv.R
test_that("my_rf_cv outputs a numeric", {
  expect_is(my_rf_cv(5), "numeric")
})

test_that("non-numeric input throws an error", {
  expect_error(my_rf_cv("a string"))
})
