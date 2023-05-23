#' Kyle likes things
#'
#' @param obj the thing kyle likes
#'
#' @return string
#' @export
#'
#' @examples
#' 
#' kyle()
#' kyle("pbj")
kyle <- function(obj = "R") {
  paste("Kyle likes", obj, sep = " ")
}

