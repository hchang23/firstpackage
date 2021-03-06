% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_knn_cv.R
\name{my_knn_cv}
\alias{my_knn_cv}
\title{k-Nearest Neighbors Cross-Validation}
\usage{
my_knn_cv(train, cl, k_nn, k_cv)
}
\arguments{
\item{train}{A data frame of the covariates (elements are numeric).}

\item{cl}{True class value of the training data.}

\item{k_nn}{Integer of the number of neighbors.}

\item{k_cv}{Integer of the number of folds.}
}
\value{
A list containing:

\code{class}, a vector of the predicted class.

\code{CV_error}, a numeric of the average cross-validation
  misclassification error rate.
}
\description{
This function randomly splits data into k "folds" and predicts class using
covariates.
}
\examples{
data(iris)

# 5-nearest neighbor CV with iris data
my_knn_cv(iris[, 1:4], iris$Species, k_nn = 5, k_cv = 5)

}
\keyword{prediction}
