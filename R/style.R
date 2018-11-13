####################################################################
# Author: Daniel Reed (daniel@danielreed.org)
# Purpose: Style functions
# Date: Wed Feb 14 21:37:21 2018
####################################################################

#' Simple theme for ggplot2 figures
#'
#' @param ... Arguments passed to \code{theme} function of \code{ggplot2}
#' @seealso \code{\link{ggplot2}{theme}}
#' @export
theme_ocean <- function(...){
  ggplot2::theme(text = ggplot2::element_text(family = "serif"),
                 panel.background = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(fill = NA, size = 1),
                 axis.title = ggplot2::element_text(size = 24),
                 axis.text = ggplot2::element_text(size = 16),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(size = 16)
                 ) + ggplot2::theme(...)
}
