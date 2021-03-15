#' Create log difference distribution
#'
#' @description Calculate log distribution and calculate difference within rows
#'
#' `r lifecycle::badge('stable')`
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

