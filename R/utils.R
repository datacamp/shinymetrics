#' S3 method to get the value of an object
#'
#' @param x an object whose value is to be retrieved
#' @param ... additional parameters passed on to the method.
#' @export
get_value <- function(x, ...){
  UseMethod('get_value', x)
}

#' @export
get_value.default <- function(x, ...){
  return(x)
}

#' @export
get_value.reactive <- function(x, ...){
  x()
}
