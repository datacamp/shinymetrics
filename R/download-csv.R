#' Download Data as CSV
#'
#' A shiny module that adds a download button to download data as a CSV.
#'
#' @param input standard \code{shiny} boilerplate
#' @param output standard \code{shiny} boilerplate
#' @param session standard \code{shiny} boilerplate
#' @param dataset a data frame, or a function/reactive that returns a data frame
#' @param filename a string, or a function/reactive that returns a string
#' @param ... additional parameters to pass to \code{\link[utils]{write.csv}}
#' @export
#' @examples
#' \dontrun{
#'   shinybones::preview_module(download_csv,
#'     dataset = mtcars,
#'     filename = function(){
#'       paste0('mtcars-', format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), '.csv')
#'     }
#'   )
#' }
download_csv <- function(input, output, session, dataset, filename, ...){
  output$data <- shiny::downloadHandler(
    filename = filename,
    content = function(file){
      utils::write.csv(get_value(dataset), file, row.names = FALSE, ...)
    }
  )
}

#' @param id a string indicating the id to use the module with.
#' @rdname download_csv
#' @export
download_csv_ui <- function(id, ...){
  ns <- shiny::NS(id)
  shiny::downloadButton(ns('data'), label = ' CSV')
}
