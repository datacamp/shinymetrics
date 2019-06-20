#' Title with help text in an informative modal, popover or tooltip
#'
#' @export
#' @param title title
#' @param help_text help_text
#' @param help_title help_title
#' @param is_h3 a boolean indicating if title should be wrapped in \code{h3}
#' @param ... additional argument passed on to \code{\link[bsplus]{bs_modal}}
#' @importFrom bsplus bs_attach_modal bs_modal
#' @importFrom commonmark markdown_html
#' @import htmltools
title_with_modal <- function(title, help_text, help_title = title,
    is_h3 = TRUE, ...){
  id <- .generate_id()
  modal_id <- paste0(id, '-help')
  help_modal <- span(class = "dc-help", style='cursor:pointer;',
      shiny::icon('question-circle-o')
    ) %>%
    bsplus::bs_attach_modal(modal_id)
  tagList(
    if (is_h3) {
      shiny::tags$h3(title, class = 'box-title', help_modal)
    } else {
       shiny::tagList(title, help_modal)
    },
    bsplus::bs_modal(
      id = modal_id,
      title = help_title,
      body = htmltools::HTML(
        commonmark::markdown_html(help_text)
      ),
      ...
    )
  )
}

#' @export
#' @rdname title_with_modal
#' @importFrom bsplus bs_embed_tooltip use_bs_tooltip
title_with_tooltip <- function(title, help_text, ...){
  tagList(
    h3(title, class = 'box-title',
      span(class = 'dc-help',
        shiny::icon('question-circle-o') %>%
          bsplus::bs_embed_tooltip(title = help_text, ...)
      )
    ),
   shiny::singleton(bsplus::use_bs_tooltip())
  )
}


#' @rdname title_with_modal
#' @export
#' @importFrom bsplus bs_embed_popover use_bs_popover
title_with_popover <- function(title, help_text, help_title = title, ...){
  shiny::tagList(
    shiny::h3(title, class = 'box-title',
       shiny::span(class = 'dc-help', style='cursor:pointer',
         shiny::icon('question-circle-o') %>%
           bsplus::bs_embed_popover(title = help_title, content = help_text, ...)
       )
    ),
    shiny::singleton(bsplus::use_bs_popover())
  )
}

.generate_id <- function(){
  paste(c("id", sample(c(letters, 1:10), 20, replace = TRUE)), collapse = "")
}
