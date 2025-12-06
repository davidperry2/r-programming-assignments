# Silence R CMD check notes about non-standard evaluation column names
utils::globalVariables(c("minutes_to_close", "reported_using_mobile"))

#' Summarize 311 requests by a grouping variable
#'
#' @param data A cleaned 311 data frame.
#' @param group_vars Character vector of columns to group by.
#'
#' @return A civics311_summary object.
#' @export
summarize_requests <- function(data,
                               group_vars = "request_type") {

  if (!is.data.frame(data)) {
    stop("summarize_requests() needs a data frame.")
  }

  if (!all(group_vars %in% names(data))) {
    stop("Some group_vars were not found in the dataset.")
  }

  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(group_vars)))

  summary_tbl <- dplyr::summarise(
    grouped,
    n_requests = dplyr::n(),
    avg_minutes_to_close = mean(minutes_to_close, na.rm = TRUE),
    pct_mobile = mean(reported_using_mobile, na.rm = TRUE),
    .groups = "drop"
  )

  out <- list(
    data = summary_tbl,
    group_vars = group_vars
  )

  class(out) <- "civics311_summary"
  out
}

#' Print method for civics311_summary
#'
#' @param x A civics311_summary object.
#' @param ... Additional arguments passed to print (unused).
#'
#' @export
print.civics311_summary <- function(x, ...) {
  cat("Summary of 311 requests grouped by:",
      paste(x$group_vars, collapse = ", "),
      "\n\n")
  print(x$data)
  invisible(x)
}
