#' Get default plot_type
#'
#' @param metric_id metric id
#' @param plot_type plot type
#' @importFrom tidymetrics keep_dimensions discard_dimensions
#' @importFrom scales hue_pal
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' get_plot_type('pct_avg_rating')
#' get_plot_type('nb_started')
#' get_plot_type('nb_started', 'line')
#' }
get_plot_type <- function(metric_id, plot_type = NULL){
  if (!is.null(plot_type)){
    return(plot_type)
  }
  line_types <- "^(pct|avg|min|median|max)_"
  if (stringr::str_detect(metric_id, line_types)) {
    "line"
  } else {
    "bar"
  }
}

custom_palette <- function(n){
  pal <- c("#8468c4", "#33AACC", "#263E63")
  if (n <= 3) {
    grDevices::colorRampPalette(rev(pal), space = "rgb")( n )
  } else {
    c(
      c("#263E63", "#8468c4"),
      scales::hue_pal(h = c(0, 360), l = 65, c = 120)(n - 2)
    )
  }
}

#  Get colors for a dimension in a metric object
#
# @param metric A \code{tbl_metric} object.
# @param dimension A string indicating the dimension.
get_colors <- function(metric_processed, dimension){
  levels <- levels(metric_processed[[dimension]])
  colors <- custom_palette(length(levels))
  if ('All' %in% levels){
    colors[levels == 'All'] <- 'black'
  }
  return(colors)
}

#  Relevel metric dimensions
#
#  NOTE: This function can be applied during metric creation if the
#  levels are based on a static period and date range.
relevel_dimension <- function(metric, dimension){
  dimension_sym <- rlang::sym(dimension)
  metadata <- attr(metric, 'metadata')
  levels <- metadata$dimensions %>%
    purrr::map('levels') %>%
    magrittr::extract2(dimension)
  if (is.null(levels)){
    metric %>%
      dplyr::mutate(!!dimension := forcats::fct_reorder(
        !!dimension_sym, .data$value, mean, .desc = TRUE
      ))
  } else if (!is.null(names(levels))){
    metric  %>%
      dplyr::mutate(!!dimension :=
        factor(!!dimension_sym, levels = names(levels))
      )
  } else {
    metric
  }
}

preprocess_data <- function(metric,
                            dimension = 'all',
                            keep_attribute_all = TRUE,
                            show_pct = FALSE,
                            quietly = FALSE){
  dimension_sym <- rlang::sym(dimension)
  d <- if (dimension == "all"){
    metric %>%
      tidymetrics::discard_dimensions(quietly = quietly)
  } else {
    metric %>%
      tidymetrics::keep_dimensions(!!dimension,
        keep_attribute_all = keep_attribute_all,
        quietly = quietly
      ) %>%
      relevel_dimension(dimension)
  }
  if (show_pct){
    d <- d %>%
      dplyr::as_data_frame() %>%
      dplyr::group_by(.data$date, .data$period) %>%
      dplyr::mutate(value = .data$value / sum(.data$value, na.rm = TRUE))
  }
  return(d)
}


# Get options to format x-axis
get_xaxis_opts <- function(metric){
  period <- metric$period[1]
  nb_dates <- metric %>%
    dplyr::distinct(date) %>%
    dplyr::pull(date) %>%
    length()
  l <- if (period %in% c("month", "quarter")){
    if (nb_dates <= 6){
      if (period == 'month'){
        list(tickformat = '%b %Y', dtick = "M1")
      } else {
        list(tickformat = '%b %Y', dtick = "M3")
      }
    } else {
      list(tickformat = '%b %Y')
    }
  } else {
    list()
  }
  l$title = ""
  return(l)
}

# Get options to format y-axis
get_yaxis_opts <- function(metric, show_pct = FALSE){
  metric_name <- attr(metric, 'metadata')$metric
  o <- list()
  o <- if (show_pct){
    list(
      tickformat = ',.0%',
      range = c(0, 1)
    )
  } else if (grepl("^pct\\_", metric_name)){
    list(
      tickformat = ',.0%',
      rangemode = "tozero"
    )
  } else {
    list(
      tickprefix = if (grepl("^usd\\_", metric_name)) "$" else "",
      range = c(0, Inf)
    )
  }
  o$title = ""
  # o$nticks = 5
  return(o)
}

expand_metric <- function(metric){
  metadata <- attr(metric, 'metadata')
  if (!is.null(metadata)){
    class(metric) <- class(metric)[class(metric) != "tbl_metric"]
    c(list(data = metric), metadata)
  } else {
    metric
  }
}

expand_metrics <- function(metrics){
  if (!('data' %in% metrics[[1]])){
    metrics %>%
      purrr::map(expand_metric)
  } else
    metrics
}
