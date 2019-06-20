#' Preview a metric in a shinydashboard
#'
#' @importFrom rlang quo quo_name
#' @export
#' @examples
#' library(dplyr)
#' \dontrun{
#' metric_satisfaction <- datacampr::tbl_metric_product_survey_avg_satisfaction()
#' preview_metric({
#'   metric_satisfaction %>%
#'   filter(period == 'rolling_28d')
#' })
#' metric <- datacampr::tbl_metric_content_courses_avg_rating_wtd()
#' preview_metric({
#'   metric %>%
#'     filter(period == 'rolling_28d')
#' })
#' preview_metric({
#'   d <- metric %>%
#'     filter(period == 'rolling_28d') %>%
#'     filter(launch_status == "live") %>%
#'     filter(technology == "R")
#' })
#' }
#' @param metric a \code{tbl_metric} object
#' @param ... additional parameters passed to \code{metric_panel}
preview_metric <- function(metric, ...){
  shinybones::preview_module('metric_panel', metric = metric, ...)
}
