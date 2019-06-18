#' Create a picker input to select aggregation period
#'
#' @param inputId the input slot that will be used to access the value.
#' @param selected_period a string indicating selected period
#' @param periods a named vector of periods.
#' @param label a string to display as label.
#' @param select_func a select input function. Either
#'   \code{\link[shiny]{selectInput}} or \code{\link[shinyWidgets]{pickerInput}}
#' @param ... additional parameters passed on to \code{select_func}
#' @export
#' @examples
#' \dontrun{
#' input_select_period('period') %>%
#'   shinybones::preview_component()
#' input_select_period('period', selected_period = 'week') %>%
#'   shinybones::preview_component()
#' }
input_select_period <- function(inputId,
                                selected_period = NULL,
                                periods = 'All',
                                label = 'aggregated_by',
                                select_func = shiny::selectInput,
                                ...){
  select_func(
    inputId,
    label = label,
    choices = period_presets_vec(get_value(periods)),
    selected = selected_period,
    ...
  )
}

period_presets_vec <- function(periods = "All"){
  periods_all <-   c(
    "Day" = "day", "Week" = "week",
    "Month" = "month", "Quarter" = "quarter",
    "Rolling 7 Day" = 'rolling_7d', "Rolling 28 Day" = "rolling_28d",
    "Rolling 56 Day" = "rolling_56d"
  )
  if (periods == 'All'){
    periods_all
  } else {
    periods_all[periods_all %in% periods]
  }
}
