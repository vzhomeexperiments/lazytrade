#' Create log difference distribution
#'
#' @description Transforms dataframe to the matrix
#'
#' @param x - matrix with one or more column
#'
#' @return - dataframe
#' @export
#'
#' @examples
#'
#' library(magrittr)
#' library(lazytrade)
#' m <- seq(1:1000) %>% as.matrix(10) %>% dlog()
#'
#'
dlog <- function(x) {


  Step2 <- diff(log(x))

  return(Step2)
}

