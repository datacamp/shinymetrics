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

# Copied over from tidymetrics
var_names_dimensions <- function(tbl){
  set1 <- tbl %>%
    dplyr::ungroup() %>%
    dplyr::select_if(~ is.character(.x) || is.factor(.x)) %>%
    colnames() %>%
    setdiff(c('date', 'value', 'period', 'metric'))

  set2 <- stringr::str_subset(colnames(tbl), "_id$")

  dplyr::union(set1, set2)
}
