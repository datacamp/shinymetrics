#' Get dimension tabs
#'
#' @param metric a \code{tbl_metric} object
#' @param hidden_dimensions a vector of dimensions that should be hidden
#' @examples
#' library(dplyr)
#' \dontrun{
#' metrics_condensed <- datacampr::dc_s3_read(
#'  'metrics_condensed.rds'
#' )$metrics_condensed
#' metrics_condensed$finance_churn_pct_b2c_churn %>%
#'   get_dimension_tabs()
#' }
get_dimension_tabs <- function(metric, hidden_dimensions = NULL){
  metric_expanded <- metric %>%
    get_value() %>%
    expand_metric()
  dimensions <- metric_expanded$dimensions
  dimension_cols <- metric_expanded$data %>%
    var_names_dimensions()
  nb_dimensions <- metric_expanded$data %>%
    tidymetrics::discard_dimensions(quietly = TRUE) %>%
    NROW()
  has_dimensions = isTRUE(nb_dimensions > 0)

  d <- dimensions %>%
    rev() %>%
    purrr::map(~ {
      .x$title <- coalesce(.x$title, .x$metric)
      .x$description <- coalesce(.x$description, .x$title)
      .x
    }) %>%
    iterate_list() %>%
    purrr::keep(~ .$name %in% dimension_cols)

  if (has_dimensions){
    append_dimension_all(d)
  } else {
    d
  }
}

# Iterate over a list
# NOTE: This is copied over from whisker::iteratelist
iterate_list <- function(x, name = 'name', value = 'value'){
  x <- as.list(x)
  nms <- names(x)
  lapply(seq_along(x), function(i) {
    l <- list()
    l[name] <- nms[i]
    l[value] <- x[i]
    l
  })
}

# Append the dimension 'all'
append_dimension_all <- function(x){
  if (length(x) > 1) {
    append(x, list(
      list(
       name = 'all',
       value = list(title = 'All', description = 'All')
      )
    ))
  } else {
    x
  }
}
