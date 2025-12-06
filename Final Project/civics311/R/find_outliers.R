# Helper: basic Tukey outlier check
tukey_outlier <- function(x, k = 1.5) {
  if (!is.numeric(x)) {
    stop("tukey_outlier() needs a numeric vector.")
  }

  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1

  lower <- q1 - k * iqr
  upper <- q3 + k * iqr

  x < lower | x > upper
}

#' Flag outliers in a numeric column of a 311 dataset
#'
#' @param data 311 data frame.
#' @param column Column name to check for outliers.
#' @param k Multiplier for the IQR. Default is 1.5.
#'
#' @return Logical vector, TRUE where x is considered an outlier.
#' @export
find_outliers <- function(data, column = "minutes_to_close", k = 1.5) {

  if (!is.data.frame(data)) {
    stop("find_outliers() expects a data frame.")
  }

  if (!column %in% names(data)) {
    stop("Column not found in dataset.")
  }

  x <- data[[column]]

  if (!is.numeric(x)) {
    stop("Only numeric columns can be checked for outliers.")
  }

  tukey_outlier(x, k = k)
}
