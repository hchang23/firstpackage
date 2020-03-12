#' Random Forest Cross-Validation
#'
#' This function uses the \code{my_gapminder} data to predict \code{lifeExp}
#' using covariate \code{gdpPercap} using folds.
#'
#' @import randomForest magrittr stats
#'
#' @param k Integer of the number of folds.
#' @keywords prediction
#'
#' @return A numeric for the cross-validation error.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  if (is.numeric(k) == FALSE) {
    stop("k must be numeric")
  }

  # sample size
  n <- nrow(my_gapminder)
  # randomly assign observations to variable folds
  folds <- sample(rep(1:k, length = n))
  # empty vector
  mse <- vector()

  for(i in 1:k) {
    # x_i
    data_train <- my_gapminder[folds != i, ]
    # x_i*
    data_test <- my_gapminder[folds == i, ]
    # y_i*
    cl_test <- my_gapminder$lifeExp[folds == i]

    # random forest model
    forest_model <- randomForest(lifeExp ~ gdpPercap, data = data_train,
                                 ntree = 100)
    # predict lifeExp of each fold
    pred <- predict(forest_model, data_test[, -4])

    # calculate and add the ith average MSE into vector
    mse <- c(mse, mean((pred - cl_test)^2))
    mse <- mean(mse)
  }
  return(mse)
}
