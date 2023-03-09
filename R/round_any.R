#' round_any
#'
#' @param x
#' @param accuracy
#' @param roundto
#'
#' @return
#' @export
#'
#' @examples

round_any <- function(thisnumber, accuracy, f=round){

  newthisnumber= f(thisnumber/ accuracy) * accuracy
  return (newthisnumber)
  }
