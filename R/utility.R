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

#' Check factor column
#' @param x factor
check.factor <- function(x){
  if(!is.factor(x))stop("Nope – not a factor")
  message("Factor with levels ", paste(levels(x), collapse = ","))
  message(sum(!is.na(x)), " values, ", sum(is.na(x)), " NAs (", length(x), " total)")
}

#' Check numeric column
#' @param x numeric
#' @return No return values - a box/dotplot is produced to examine values
check.numeric <- function(x){
  if(!is.numeric(x))stop("Feed me numerics not ", class(x), "s")

  message("See plot...")

  ggplot2::ggplot(data.frame(y = x), aes(x = 1, y = y)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_dotplot(binaxis = "y", stackdir = "center") +
    ggplot2::ylab("Values") +
    ggplot2::xlab("") +
    kitchendraw::theme_ocean(axis.text.x = ggplot2::element_blank())
}

#' Check character column
#' @param x numeric
#' @param n number of random values to show – 20 by default
check.character <- function(x, n = 20){
  if(!is.character(x))stop("Expected characters, got ", class(x), "s. Disappointed.")
  message("Character with ", length(unique(x)), " unique values")
  message(sum(!is.na(x)), " values, ", sum(is.na(x)), " NAs (", length(x), " total)")
  message("Random sample of values:\n", paste(" ", sample(x, min(length(x), n)), "\n", sep = ""))
}

#' Dispatch function for checking
#' @param variable to be 'checked'
check <- function(x, ...){
  UseMethod("check", x)
}

#' Summarise each column of a data frame
#'
#' @param df The data frame to be summarised
#' @return Returns \code{NULL}, but outputs figures and summary tables
#' @export
check_df <- function(df){
  # Check that df is a data frame
  if(!is.data.frame(df))stop("What are you trying to pull here?! check_df expects a data frame")

  # Get column names
  df_cols <- colnames(df)

  # Check all columns
  invisible(lapply(df_cols, function(x){print(x, quote = FALSE); check(df[[x]])}))
}
