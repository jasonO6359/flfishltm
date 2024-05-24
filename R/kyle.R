#' Kyle likes things
#'
#' @param obj *string* the thing kyle likes
#'
#' @return string
#' @noRd
#'
#' @examples
#' 
#' kyle()
#' kyle("pbj")
kyle <- function(obj = "R") {
  paste("Kyle likes", obj, sep = " ")
}

