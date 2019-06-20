#' Display a metric in a panel
#'
#'
#' @export
#' @param id a string indicating the id to call the module with
#' @param input standard \code{shiny} boilerplate
#' @param output standard \code{shiny} boilerplate
#' @param session standard \code{shiny} boilerplate
#' @param metric A metric to display. It should be a list with data and title
#' @param plot_type Either "line" or "bar": if neither is given, it guesses
#' "line" for \code{pct}, \code{avg}, \code{min}, \code{median}, and \code{max}
#' metrics, and "bar" otherwise. Ignored if \code{plot_fun} is provided.
#' @param plot_fun A plotting function that takes two arguments - data and
#'   dimension
#' @param plot_post_process a function to post-process the default plot
#' @param orientation a string indicating orientation (vertical or horizontal)
#' @param selected_date_range_preset Default date range preset to use. Use
#'   \code{date_range_presets_vec()} to look up possible presets
#' @param selected_period Default period to display the data
#' @param height height of the panel
#' @param hidden_dimensions A vector of dimension names that should not be
#'   displayed as tabs.
#' @param div_bottom_left html to display on bottom left of the panel
#' @param ... Additional parameters passed to the server
#' @examples
#' library(dplyr)
#' \dontrun{
#' metrics_condensed <- datacampr::dc_s3_read(
#'   "metrics_condensed.rds"
#' )$metrics_condensed
#' metric <- metrics_condensed$finance_forecasts_usd_arr_total %>%
#'   filter(period == 'week')
#' shinybones::preview_module(metric_panel, metric = metric, plot_type = 'line')

#' preview_metric(metrics_condensed$finance_cash_usd_cash_in)
#' preview_metric(flights_nyc_avg_arr_delay)
#' }
#' @importFrom shinycssloaders withSpinner
#' @importFrom tidymetrics discard_constant_dimensions
#' @export
metric_panel <- function(input, output, session,
                         metric,
                         plot_type = NULL,
                         plot_fun = NULL,
                         plot_post_process = NULL,
                         orientation = 'vertical',
                         selected_date_range_preset = "All Time",
                         selected_period = "Week",
                         height = 400,
                         div_bottom_left = NULL,
                         hidden_dimensions = NULL,
                         ...){

  ns = session$ns
  metric <- purrr::possibly(
    tidymetrics::discard_constant_dimensions, metric
  )(metric)

  rv_metric_filtered <- callModule(metric_panel_footer, 'metric_filtered',
    metric = metric,
    date_range = c(Sys.Date() - 365, Sys.Date()),
    selected_date_range_preset = selected_date_range_preset,
    selected_period = selected_period
  )

  if (is.null(div_bottom_left)){
    div_bottom_left <- show_as_tags(attr(metric, 'metadata')$dimensions_filters)
  }
  dimension_tabs <- metric %>%
    get_dimension_tabs(hidden_dimensions)

  dimension_tabs %>%
    lapply(function(x){
      output[[paste0('plot_', x$name)]] <- plotly::renderPlotly({
        plot_metric_condensed(
          metric = rv_metric_filtered(),
          plot_type = plot_type,
          dimension = x$name
        )
      })
    })

  output$ui_tabs <- shiny::renderUI({
    tab_box <- metric_panel_ui_tabs(
       ns, metric, height = height, orientation = orientation,
       div_bottom_left = div_bottom_left
    )
    tagList(
      div(class = 'col-sm-12', style = 'margin-bottom:15px', tab_box),
      div(class = 'col-sm-12', metric_panel_footer_ui(
        ns('metric_filtered'),
        selected_period = selected_period,
        periods = metric %>%
          dplyr::distinct(period) %>%
          dplyr::pull(period)
      ))
    )
  })
}

#' @export
#' @rdname metric_panel
metric_panel_ui <- function(id, ...){
  ns <- shiny::NS(id)
  shiny::uiOutput(ns('ui_tabs'))
}

metric_panel_ui_tabs <- function(ns, metric, height = 400,
                                 orientation = 'vertical',
                                 div_bottom_left = div_bottom_left,
                                 ...){
  tabs <- get_dimension_tabs(metric)
  metadata <- attr(metric, 'metadata')
  title <- metric_panel_ui_title(metadata, ns)
  tab_selected <- utils::tail(tabs, 1)[[1]]$name
  tab_panels <- tabs %>%
    get_value() %>%
    purrr::map(~ {
      id = paste0('plot_', .x$name)
      shiny::tabPanel(
        id = id,
        value = .x$name,
        title = tags$span(.x$value$title) %>%
          bsplus::bs_embed_tooltip(.x$value$description),
        tagList(
          plotly::plotlyOutput(ns(id), height = height) %>%
            shinycssloaders::withSpinner(),
          div(
            div_bottom_left,
            div(class = 'pull-right', text_updated_at(metadata$updated_at))
          )
        )
      )
    }) %>%
    append(
      list(
        title = title, side = "right", selected = tab_selected,
        width = NULL,
        id = ns('dimension'), height = height + 50
      )
    )
  do.call(shinydashboard::tabBox, tab_panels)
}

metric_panel_ui_title <- function(metadata, ns = shiny::NS(NULL)){
  title = title_with_modal(
    metadata$title,
    help_title = tags$span(
      metadata$title,
      shiny::tags$a(
        shiny::icon('code'), href = metadata$rmd_link, target = "_blank"
      )
    ),
    help_text = metadata$description,
    # footer = dcdash:::enhanced_footer(ns, metadata),
    is_h3 = FALSE
  )
}
