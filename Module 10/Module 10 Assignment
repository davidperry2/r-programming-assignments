#' Generate Friedman Synthetic Data
#'
#' This function generates the Friedman #1 dataset, a popular benchmark
#' for testing regression and machine learning models.
#'
#' @param n Number of observations to generate.
#' @param noise Standard deviation of Gaussian noise.
#' @return A data frame containing predictors X1–X10 and response y.
#' @examples
#' data <- friedman_generate(200)
#' head(data)
#' @export
friedman_generate <- function(n = 200, noise = 1.0) {
  X <- matrix(runif(n * 10, min = 0, max = 1), ncol = 10)
  y <- 10 * sin(pi * X[,1] * X[,2]) +
    20 * (X[,3] - 0.5)^2 +
    10 * X[,4] + 5 * X[,5] +
    rnorm(n, 0, noise)
  df <- as.data.frame(X)
  names(df) <- paste0("X", 1:10)
  df$y <- y
  return(df)
}
