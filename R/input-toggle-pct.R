#' Radio buttons to toggle between number and percentage.
#'
#' @param inputId the input slot that will be used to access the value.
#' @param label label to display alongside the buttons
#' @param size  size of the radio buttons ('sm', 'xs', or 'lg')
#' @param ... additional parameters to pass to
#'   \code{\link[shinyWidgets]{radioGroupButtons}}
#' @examples
#' \dontrun{
#'   input_toggle_pct('show_pct') %>%
#'     shinybones::preview_component()
#' }
input_toggle_pct <- function(inputId, label = 'as', size = 'sm', ...){
  shinyWidgets::radioGroupButtons(
    inputId = inputId,
    label = "as",
    choices = c(
      `<i class='fa fa-bar-chart'></i>` = "number",
      `<i class='fa fa-percent'></i>` = "percent"
    ),
    checkIcon = list(
      yes = shiny::icon("ok", lib = 'glyphicon')
    ),
   size = "sm",
   ...
 )
}
