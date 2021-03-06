# within test-my_knn_cv.R
test <- my_knn_cv(iris[, 1:4], iris$Species, 1, 5)

test_that("my_knn_cv outputs a list", {
  expect_is(test, "list")
})

test_that("non-data input throws error", {
  expect_error(my_knn_cv("a string"))
})
