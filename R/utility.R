####################################################################
# Author: Daniel Reed (daniel@danielreed.org)
# Purpose: Helper functions for data wrangling
# Date: Tue Sep 04 10:35:57 2018
####################################################################

#' Remove all attributes except class.
#'
#' @param vec A data frame column or vector
#' @return Return \code{vec} with no attributes except \code{class}
#' @export
strip <- function(vec){
  # Attribute names
  att_names <- names(attributes(vec))

  # Remove class from list of attribute names
  att_names <- att_names[att_names != "class"]

  # Check that there are attributes; if not return original vector
  if(length(att_names) == 0)return(vec)

  # Loop through attributes and remove
  for(i in att_names){
    attr(vec, i) <- NULL
  }

  # Return vector
  vec
}

#' Calculate proportions of two water masses in a third (target) water mass using a conservative tracer.
#'
#' @param t1 Tracer in first water mass
#' @param t2 Tracer in second water mass
#' @param tT Tracer of target water mass
#' @return Return a data frame containing proportion of two water masses calculated from temperature & salinity.
#' @export
wm <- function(t1, t2, tT){
  alpha <- (tT - t2)/(t1 - t2)

  return(data.frame(alpha = alpha, beta =  1 - alpha))
}


#' Convert a function into a conditional function
#'
#' @param fun Name of function to convert
#' @return Function \code{fun} with an additional logical parameter \code{execute} that toggles whether it is
#' executed or not.
#'
#' @description This function takes another function \code{fun} and adds a logical parameter called \code{execute} that
#' that specifies whether the function is executed or not. The function was created by \emph{ablack3} and posted
#' \href{https://github.com/tidyverse/magrittr/issues/109#issuecomment-371999796}{here} on GitHub.
#'
#' @export
conditionally <- function(fun){
  function(first_arg, ..., execute){
    if(execute) return(fun(first_arg, ...))
    else return(first_arg)
  }
}
