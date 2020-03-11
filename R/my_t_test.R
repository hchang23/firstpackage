#' t-test
#'
#' This function performs one-sample t-tests on vectors of data.
#'
#' @param x Numeric vector of data values.
#' @param alternative Character string of the alternative hypothesis, must be
#'   either "\code{two.sided}", "\code{less}", or "\code{greater}".
#' @param mu Numeric of true value of the mean.
#' @keywords inference
#'
#' @return A list containing the following:
#' @return \code{t_stat}, the value of the test statistic.
#' @return \code{df}, the degrees of freedom for the t-statistic.
#' @return \code{alternative}, the alternative hypothesis.
#' @return \code{p_val}, the p-value for the test.
#'
#' @examples
#' my_t_test(1:10, "two.sided", 0)
#' my_t_test(rnorm(10), "less", 0)
#'
#' @export
my_t_test <- function(x, alternative, mu) {
  if (alternative != "two.sided" &
      alternative != "less" &
      alternative != "greater") {
    # error message
    stop("alternative must be char string 'two.sided', 'less', or 'greater'")
  }

  # sample size n is number of elements in x vector
  n <- length(x)
  # degrees of freedom (df) is n - 1
  df <- n - 1
  # calculate standard error
  se <- sd(x) / sqrt(n)
  # calculate test statistic
  test_stat <- (mean(x) - mu) / se

  # calculate p-value depending on alternative hypothesis
  if (alternative == "two.sided") {
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE) * 2
  } else if (alternative == "less") {
    p_val <- pt(test_stat, df)
  } else {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  }

  result_list <- list(t_stat = test_stat, df = df, alternative = alternative,
                      p_val = p_val)
  return(result_list)
}
