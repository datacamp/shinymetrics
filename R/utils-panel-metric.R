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
#' @importFrom humanize natural_time
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
      .x$title <- dplyr::coalesce(.x$title, .x$metric)
      .x$description <- dplyr::coalesce(.x$description, .x$title)
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

show_as_tags <- function(x){
  if (is.null(x)) return(shiny::span(shiny::HTML("&nbsp;")))
  x_colors <- custom_palette(length(x))
  names(x_colors) <- x
  x %>%
    purrr::map(~ {
      shiny::tags$span(.x,
        class = 'label',
        style = sprintf('background-color:%s', x_colors[.x])
      )
    }) %>%
    shiny::tagList()
}


text_updated_at <- function(updated_at){
  if (is.null(updated_at)){
    updated_at <- 'Last updated: Unknown'
    return(shiny::tags$small(class = 'text-danger', updated_at))
  }
  time_elapsed = as.numeric(
    difftime(Sys.time(), updated_at, units = 'hours')
  )
  updated_at <- humanize::natural_time(updated_at)
  updated_at <- paste('Last updated:', updated_at)
  if (time_elapsed >= 48){
    shiny::tags$small(class = 'text-danger', updated_at)
  } else if (time_elapsed >= 24){
    shiny::tags$small(class = 'text-warning', updated_at)
  } else {
    shiny::tags$small(updated_at)
  }
}
