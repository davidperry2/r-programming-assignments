#' Clean basic 311 service request data
#'
#' This function tries to clean up common formatting issues in 311
#' datasets: messy column names, date formats, and logical flags.
#'
#' @param data A data frame containing 311 request data.
#'
#' @return A tibble with cleaned fields.
#' @export
clean_311 <- function(data) {

  # Quick safety check
  if (!is.data.frame(data)) {
    stop("clean_311() expects a data frame.")
  }

  # Convert to tibble so printing is nicer
  data <- dplyr::as_tibble(data)

  # Clean column names a bit
  names(data) <- names(data) |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_to_lower()

  # Try to parse some typical date columns if they exist
  date_cols <- c("service_request_date", "last_updated", "closed")
  for (col in date_cols) {
    if (col %in% names(data)) {
      # ymd_hms() returns NA for invalid formats, which is fine
      data[[col]] <- lubridate::ymd_hms(data[[col]], quiet = TRUE)
    }
  }

  # Convert typical TRUE/FALSE fields
  logical_cols <- c("acknowledged", "reopened", "reported_using_mobile")
  for (col in logical_cols) {
    if (col %in% names(data)) {
      data[[col]] <- as.logical(data[[col]])
    }
  }

  # Convert duration columns to numeric if present
  duration_cols <- c("minutes_to_acknowledge", "minutes_to_close", "days_to_close")
  for (col in duration_cols) {
    if (col %in% names(data)) {
      data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
    }
  }

  data
}
