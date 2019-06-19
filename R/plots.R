#' Plot a condensed metric
#'
#' @param metric a \code{tbl_metric} object
#' @param plot_type a string indicating type of plot (line/bar)
#' @param dimension a string indicating the dimension to visualize
#' @param quietly a boolean indicating if messages should be suppressed
#' @param ... additional parameters passed
#' @examples
#' \dontrun{
#'   library(dplyr)
#'   metrics_condensed <- datacampr::dc_s3_read(
#'     'metrics_condensed.rds'
#'   )$metrics_condensed

#'   metric %>%
#'     filter(period == 'week') %>%
#'     filter(date >= Sys.Date() - 365) %>%
#'     plot_metric_condensed()
#'
#'  metrics_condensed$product_time_median_time_spent %>%
#'    filter(period == 'week') %>%
#'    filter(date >= Sys.Date() - 365) %>%
#'    plot_metric_condensed_line(dimension = 'subscription_type')
#'
#'  metrics_condensed$content_courses_avg_rating_wtd %>%
#'    filter(period == 'rolling_28d') %>%
#'    filter(date >= Sys.Date() - 365) %>%
#'    plot_metric_condensed()
#'
#'  metrics_condensed$content_courses_avg_rating_wtd %>%
#'    filter(period == 'rolling_28d') %>%
#'    filter(date >= Sys.Date() - 365) %>%
#'    plot_metric_condensed('ds_track')
#'
#'  flights_nyc_avg_arr_delay %>%
#'    filter(period == 'week') %>%
#'    plot_metric_condensed(plot_type = 'bar')
#'
#'  flights_nyc_avg_arr_delay %>%
#'    filter(period == 'week') %>%
#'    plot_metric_condensed(plot_type = 'bar', dimension = 'origin')
#'
#'  flights_nyc_avg_arr_delay %>%
#'    filter(period == 'week') %>%
#'    plot_metric_condensed(plot_type = 'line')
#'
#'  flights_nyc_avg_arr_delay %>%
#'    filter(period == 'week') %>%
#'    plot_metric_condensed(plot_type = 'line', dimension = 'origin')
#' }
#' @export
#' @importFrom plotly plot_ly layout config add_lines add_bars
plot_metric_condensed <- function(metric,
                                  dimension = 'all',
                                  plot_type = NULL,
                                  quietly = TRUE,
                                   ...){
  if (is.null(plot_type)){
    metric_id <- attr(metric, 'metadata')$metric
    plot_type <- get_plot_type(metric_id)
  }
  if (plot_type == 'line'){
    plot_metric_condensed_line(metric, dimension, quietly, ...)
  } else {
    plot_metric_condensed_bar(metric, dimension, quietly, ...)
  }
}

plot_metric_condensed_line <- function(metric,
                                       dimension = "all",
                                       quietly = TRUE,
                                       ...){
  dim_sym <- rlang::sym(dimension)
  metric_processed <- metric %>%
    preprocess_data(dimension, keep_attribute_all = TRUE, quietly = quietly)

  plt <- if (dimension == 'all'){
    metric_processed %>%
      plotly::plot_ly(x = ~ date, y = ~ value, color = I('black'))
  } else {
    metric_processed %>%
      dplyr::mutate(size = dplyr::if_else(!!dim_sym == 'all', 3, 1)) %>%
      plotly::plot_ly(
        x = ~ date, y = ~ value,
        color = stats::as.formula(paste("~", dimension)),
        colors = get_colors(metric_processed, dimension),
        size = ~ size, sizes = c(1, 4)
      )
  }

  plt %>%
    plotly::add_lines() %>%
    plotly::layout(
      plot_bgcolor = "#EBF4F7",
      hovermode = "compare",
      margin = list(r = 60),
      xaxis = get_xaxis_opts(metric),
      yaxis = get_yaxis_opts(metric)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

plot_metric_condensed_bar <- function(metric,
                                      dimension = 'all',
                                      quietly = TRUE,
                                      barmode = 'stack',
                                      show_pct = FALSE,
                                      ...){
  metric_processed <- metric %>%
    preprocess_data(
      dimension,
      show_pct = show_pct,
      keep_attribute_all = FALSE,
      quietly = quietly
    )
  plt <- if (dimension == "all"){
    metric_processed %>%
      plotly::plot_ly(x = ~ date, y = ~ value, colors = '#3ac')
  } else {
    metric_processed %>%
      plotly::plot_ly(
        x = ~ date, y = ~ value,
        color = stats::as.formula(paste("~", dimension)),
        colors = get_colors(metric_processed, dimension)
      )
  }
  plt %>%
    plotly::add_bars() %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::layout(
      plot_bgcolor = "#EBF4F7",
      hovermode = "compare",
      barmode = barmode,
      legend = list(orientation = "v"),
      margin = list(r = 60),
      xaxis = get_xaxis_opts(metric),
      yaxis = get_yaxis_opts(metric, show_pct = show_pct)
    )
}
