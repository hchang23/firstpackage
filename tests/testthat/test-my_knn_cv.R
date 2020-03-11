# within test-my_knn_cv.R
data("iris")
test <- my_knn_cv(iris[, 1:4], iris$Species, 1, 5)

test_that("my_knn_cv outputs a list", {
  expect_is(test, "list")
})

test_that("elements of output list have correct class", {
  expect_vector(test$class, "vector")
  expect_is(test$CV_error, "numeric")
})
