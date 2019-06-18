#' S3 method to get the value of an object
#'
#' @param x an object whose value is to be retrieved
#' @param ... additional parameters passed on to the method.
get_value <- function(x, ...){
  UseMethod('get_value', x)
}

get_value.default <- function(x, ...){
  return(x)
}

get_value.reactive <- function(x, ...){
  x()
}

set_selected <- function(x, choices, default){
  if (!is.null(x) && (x %in% choices)){
    x
  } else {
    default
  }
}

