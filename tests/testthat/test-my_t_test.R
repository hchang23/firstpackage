# within test-my_t_test.R
test_that("alternative input that isn't 'two.sided', 'less', or 'greater'
          throws error", {
  expect_error(my_t_test(1:10, "a string", 0))
  expect_error(my_t_test(1:10, 5, 0))
})

test_that("p-value is calculated correctly", {
  expect_equal(my_t_test(1:10, "two.sided", 0)$p_val, 3e-04, tolerance = 0.001)
  expect_equal(my_t_test(1:10, "less", 0)$p_val, 0.9999, tolerance = 0.001)
  expect_equal(my_t_test(1:10, "greater", 0)$p_val, 1e-04, tolerance = 0.001)
})

test_that("my_t_test outputs a list", {
  expect_is(my_t_test(1:10, "less", 0), "list")
})

test_that("elements of output list have correct class", {
  expect_is(my_t_test(1:10, "less", 0)$t_stat, "numeric")
  expect_is(my_t_test(1:10, "less", 0)$df, "numeric")
  expect_is(my_t_test(1:10, "less", 0)$alternative, "character")
  expect_is(my_t_test(1:10, "less", 0)$p_val, "numeric")
})
