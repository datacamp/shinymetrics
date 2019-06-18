#' Create date range input with preset ranges
#'
#' This is a shiny module to create a date range input with presets for the
#' Last 1 week, 2 weeks, 6 months etc.
#'
#' @param input standard \code{shiny} boilerplate
#' @param output standard \code{shiny} boilerplate
#' @param session standard \code{shiny} boilerplate
#' @param id a string indicating the id to use the module with
#' @param date_range selected date range to display
#' @param date_range_preset selected date range preset selected
#' @param ... Additional parameters passed to the module
#' @examples
#' library(shiny)
#' \dontrun{
#'  shinybones::preview_module(input_date_range, use_box = TRUE)
#'  test_date_range <- function(input, output, session, ...){
#'    ns <- session$ns
#'    date_range_input <-  callModule(input_date_range, 'date_range')
#'    output$date_range_text <- renderText({
#'      paste(date_range_input(), collapse = " - ")
#'    })
#'  }
#'  test_date_range_ui <- function(id, ...){
#'    ns <- shiny::NS(id)
#'    shinydashboard::box(
#'      width = 12,
#'      title = 'Date Range Input',
#'      input_date_range_ui(ns('date_range')),
#'      column(12, textOutput(ns('date_range_text')))
#'    )
#'  }
#'  shinybones::preview_module(test_date_range)
#' }
#' @return A reactive vector of the selected date range
input_date_range <- function(input, output, session,
                             date_range = c(Sys.Date() - 365, Sys.Date()),
                             date_range_preset = 'Last Year',
                             ...){
  ns <- session$ns
  output$ui_date_range_custom <- shiny::renderUI({
    date_range <- get_value(date_range)
    shiny::req(input$date_range_preset)
    if (input$date_range_preset == 'custom_period'){
      shiny::dateRangeInput(
        ns('date_range'),
        label = 'from',
        start = date_range[1],
        end = date_range[2],
        min = date_range[1],
        max = date_range[2]
      )
    }
  })
  output$ui_date_range_preset <- shiny::renderUI({
    date_range_presets <- date_range_presets_vec(
      max(get_value(date_range))
    )
    choices_date_range_preset = c(
      date_range_presets,
      "Custom Period" = "custom_period"
    )
    selected_date_range_preset <- set_selected(
      input$date_range_preset,
      choices_date_range_preset,
      date_range_presets[get_value(date_range_preset)]
    )
    shinyWidgets::pickerInput(
      ns("date_range_preset"),
      label = "over",
      choices = choices_date_range_preset,
      selected = selected_date_range_preset
    )
  })

  date_range_selected <- shiny::reactive({
    shiny::req(input$date_range_preset)
    if (input$date_range_preset == 'custom_period'){
      input$date_range
    } else {
      c(input$date_range_preset, as.character(max(date_range)))
    }
  })
  return(date_range_selected)
}

#' @rdname input_date_range
input_date_range_ui <- function(id, ...){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(4, shiny::uiOutput(ns('ui_date_range_preset'))),
    shiny::column(8, shiny::uiOutput(ns('ui_date_range_custom'))),
    input_date_range_ui_css()
  )
}

input_date_range_ui_css <- function(){
  shiny::tags$style(shiny::HTML('
    .input-daterange > .input-sm {
      height:34px;
      background-color:#f4f4f4;
    }
    .input-daterange > .input-sm.form-control{
       height:34px;
       background-color:#f4f4f4;
      }
  '))
}

#' Date Range Presets
#'
#' This function returns a vector of preset date ranges like Last N Weeks.
#'
#' @param max_date The max date for date range to return
#' @importFrom dplyr case_when
#' @importFrom stringr str_c
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

