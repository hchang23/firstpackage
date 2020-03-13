#' k-Nearest Neighbors Cross-Validation
#'
#' This function randomly splits data into k "folds" and predicts class using
#' covariates.
#'
#' @import class magrittr
#'
#' @param train A data frame of the covariates (elements are numeric).
#' @param cl True class value of the training data.
#' @param k_nn Integer of the number of neighbors.
#' @param k_cv Integer of the number of folds.
#' @keywords prediction
#'
#' @return A list containing:
#' @return \code{class}, a vector of the predicted class.
#' @return \code{CV_error}, a numeric of the average cross-validation
#'   misclassification error rate.
#'
#' @examples
#' data(iris)
#'
#' # 5-nearest neighbor CV with iris data
#' my_knn_cv(iris[, 1:4], iris$Species, k_nn = 5, k_cv = 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # sample size
  n <- nrow(train)
  # randomly assign observations to variable folds
  folds <- sample(rep(1:k_cv, length = n))
  # empty vector
  misclass <- vector()

  # loop through folds
  for(i in 1:k_cv) {
    # x_i
    data_train <- train[folds != i, ]
    # x_i*
    data_test <- train[folds == i, ]
    # y_i
    cl_train <- cl[folds != i]
    # y_i*
    cl_test <- cl[folds == i]

    # predict class
    class_cv <- knn(data_train, data_test, cl_train, k_nn)
    # record prediction
    pred <- table(cl_test, class_cv)
    # add the ith misclassification rate into vector
    misclass <- c(misclass, 1 - sum(diag(pred) / (sum(rowSums(pred)))))
  }
  # compute CV error (avg misclassification rate)
  cv_err <- mean(misclass)

  # class with full data
  class <- as.vector(knn(train, train, cl, k_nn))

  # store class and cv_error into a list
  output <- list(class = class, CV_error = cv_err)
  return(output)
}
