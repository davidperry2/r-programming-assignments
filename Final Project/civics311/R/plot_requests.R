# Silence R CMD check notes about NSE variables
utils::globalVariables(c("request_type", "n_requests"))

#' Plot request counts from either a summary or raw data
#'
#' This function is a generic that dispatches to methods for
#' civics311_summary objects or raw data frames. If given raw data,
#' the function will summarize the data first.
#'
#' @param x A civics311_summary object or a raw 311 data frame.
#' @param ... Additional arguments passed to methods.
#'
#' @return A ggplot object showing request counts.
#' @export
#'
plot_requests <- function(x, ...) {
  UseMethod("plot_requests")
}

#' @importFrom stats reorder
#'
#' @param x A civics311_summary object.
#' @param ... Additional arguments (unused).
#' @export
plot_requests.civics311_summary <- function(x, ...) {
  ggplot2::ggplot(
    x$data,
    ggplot2::aes(
      x = stats::reorder(request_type, n_requests),
      y = n_requests
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Request Type",
      y = "Number of Requests",
      title = "311 Service Request Counts"
    )
}

#' @param x A raw 311 data frame.
#' @param ... Additional arguments (unused).
#' @export
plot_requests.data.frame <- function(x, ...) {
  if (!"request_type" %in% names(x)) {
    stop("The data frame must have a 'request_type' column.")
  }

  s <- summarize_requests(x)
  plot_requests(s)
}

#' Allow base plot() to work on civics311 summary objects.
#'
#' @param x A civics311_summary.
#' @param ... Additional arguments (unused).
#' @export
plot.civics311_summary <- function(x, ...) {
  print(plot_requests(x))
}
