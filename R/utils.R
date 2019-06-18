#' S3 method to get the value of an object
#'
#' @param x an object whose value is to be retrieved
#' @param ... additional parameters passed on to the method.
#' @export
get_value <- function(x, ...){
  UseMethod('get_value', x)
}

#' @export
get_value.default <- function(x, ...){
  return(x)
}

#' @export
get_value.reactive <- function(x, ...){
  x()
}


date_range_presets_vec <- function(max_date = NULL){
  weeks_back <- c(1, 2, 4, 8, 12, 26, 365 / 7, 10*365 / 7)
  dates <- as.character(Sys.Date() - as.integer(weeks_back*7))
  weeks_back <- as.integer(weeks_back)
  names(dates) <- dplyr::case_when(
    weeks_back == 1 ~ "Last Week",
    weeks_back <= 12 ~ stringr::str_c("Last ", weeks_back, " Weeks"),
    weeks_back == 26 ~ "Last 6 Months",
    weeks_back == 52 ~ "Last Year",
    weeks_back > 52 ~ "All Time"
  )
  if (!is.null(max_date)) {
    dates <- dates[dates <= max_date]
  }
  dates
}

set_selected <- function(x, choices, default){
  if (!is.null(x) && (x %in% choices)){
    x
  } else {
    default
  }
}

