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

#' Calculate proportions of two water masses in a third (target) water mass.
#'
#' @param t1 Temperature of first water mass
#' @param t2 Temperature of second water mass
#' @param tT Temperature of target water mass
#' @param s1 Salinity of first water mass
#' @param s2 Salinity of second water mass
#' @param sT Salinity of target water mass
#' @return Return a vector containing proportion of two water masses and there sum, which should be close to 1
#' @export
wm <- function(t1, t2, tT, s1, s2, sT){
  # Matrix defining LHS of equation
  A <- matrix(c(t1, t2, s1, s2), ncol = 2, byrow = TRUE)

  # Vector defining RHS of equation
  b <- c(tT, sT)

  # Solve equations
  res <- solve(A, b)

  # Return ssolution and sum of solution
  return(c(res, sum(res)))
}

