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
                 panel.grid.major = ggplot2::element_line(colour = scales::alpha(gray50, 0.5),
                                                          linetype = "dashed")
                 ) + ggplot2::theme(...)
}

#' Shepard Fairey colour palette
#'
#' @param num Number of colours required
#' @param type \code{discrete} or \code{continuous} colours
#' @return Returns colours palette with \code{num} colours
#' @export
fairey <- function(num, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  # Colours
  pal <- c("#0A2A3F", "#6593A0", "#B9CCB8", "#FFEFA7", "#DB1522")

  # If number of colours missing use them all
  if(missing(num)) {
    num <- length(pal)
  }

  # If there are fewer colours than requested STOP
  if(type == "discrete" & num > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  # If continuous, interpolate between colours
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(num),
                discrete = pal[1:num]
  )
  structure(out, class = "palette")
}
