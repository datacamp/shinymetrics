#' Preview a metric in a shinydashboard
#'
#' @importFrom rlang quo quo_name
#' @export
#' @examples
#' library(dplyr)
#' \dontrun{
#'  metric_satisfaction <- datacampr::tbl_metric_product_survey_avg_satisfaction()
#'  preview_metric(metric_satisfaction)
#'  metric_rating <- datacampr::tbl_metric_content_courses_avg_rating_wtd()
#'  preview_metric(metric_rating, selected_period = 'Rolling 28 Day')
#'  preview_metric(
#'    metric_rating %>%
#'      filter(launch_status == "live") %>%
#'      filter(technology == "R"),
#'    selected_period = 'Rolling 28 Day'
#'  )
#' }
#' @param metric a \code{tbl_metric} object
#' @param ... additional parameters passed to \code{metric_panel}
#' @export
preview_metric <- function(metric, ...){
  shinybones::preview_module(metric_panel, metric = metric, ...)
}
