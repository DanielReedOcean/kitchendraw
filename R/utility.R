####################################################################
# Author: Daniel Reed (daniel@danielreed.org)
# Purpose: Helper functions for data wrangling
# Date: Tue Sep 04 10:35:57 2018
####################################################################

#' Remove all attributes except class
#'
#' @export
strip <- function(vec){
  # Attribute names
  att_names <- names(attributes(vec))

  # Remove class from list of attribute names
  att_names <- att_names[att_names != "class"]

  # Check that there are attributes; if not return original vector
  if(length(att_names) == 0)return(vec)

  lapply(att_names, function(x)attr(vec, x) <- NULL)

  vec
}
