#' S3 method to get the value of an object
#'
#' @param x an object whose value is to be retrieved
#' @param ... additional parameters passed on to the method.
#' @importFrom methods formalArgs
#' @importFrom purrr map
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


# Utility to do map + do.call
map_call <- function(.x, .f, ...){
  purrr::map(.x, ~ do.call(.f, .x))
}

map_call_2 <- function(.x, .f, ...){
  purrr::map(.x, ~ do_call_2(.f, .x))
}

# Extend do
do_call_2 <- function (what, args, ...){
  args_what <- methods::formalArgs(what)
  args <- args[names(args) %in% c("", args_what)]
  do.call(what, args, ...)
}
