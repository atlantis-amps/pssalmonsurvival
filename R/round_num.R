#' round_num
#'
#' @param x
#' @param accuracy
#' @param roundto
#'
#' @return
#' @export
#'
#' @examples

round_num <- function(thisnumber, accuracy, f=round){

  newthisnumber= f(thisnumber/ accuracy) * accuracy
  return (newthisnumber)
  }
