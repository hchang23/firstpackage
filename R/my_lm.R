#' Fitting Linear Models
#'
#' This function is used to fit linear models on a data set.
#'
#' @param formula A "\code{formula}" class object that describes the model to
#'   be fitted, typically having the form \code{response ~ terms}.
#' @param data Data frame containing the variables in the model.
#' @keywords inference
#'
#' @return A table with rows for each coefficient and columns for the following:
#'   \code{Estimate}, \code{Std. Error}, \code{t value}, and \code{Pr(>|t|)}.
#'
#' @examples
#' # load mtcars data
#' data("mtcars")
#'
#' my_lm(mpg ~ hp, mtcars)
#' my_lm(mpg ~ hp + wt, mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # extract model matrix X
  matrix_x <- model.matrix(formula, data)
  # transpose of model matrix X
  transpose_x <- t(matrix_x)
  # extract model frame
  m_frame <- model.frame(formula, data)
  # extract model response Y using frame
  response_y <- model.response(m_frame)

  # solve for coefficients (estimate)
  beta <- solve((transpose_x %*% matrix_x)) %*% transpose_x %*% response_y

  # calculate degrees of freedom (sample size - covariates - intercept)
  df <- nrow(data) - nrow(beta)
  # estimate sigma hat squared
  sigma <- sum((response_y - matrix_x %*% beta)^2 / df)
  # calculate standard error with sigma squared
  se <- suppressWarnings(diag(sqrt(sigma * solve((transpose_x %*% matrix_x)))))

  # calculate t value
  t_val <- beta / se

  # calculate p-value (beta = 0)
  p_val <- pt(abs(t_val), df, lower.tail = FALSE) * 2

  result_table <- as.table(cbind(beta, se, t_val, p_val))
  colnames(result_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(result_table)
}
