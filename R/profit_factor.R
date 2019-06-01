#' PROFIT FACTOR FUNCTION
#'
#' @param x column vector with profit or loss of the orders for one system
#
#'
#' @return function should calculate profit factor for this vector and return one value also as vector
#' @export
#'
#' @examples
#'
#' # require(magrittr)
#' x <- read_rds('test_data.rds') %>% select(X5) %$% X5
#' x <- read_rds('test_data.rds') %>% filter(X5 > 0) %>% select(X5) %$% X5
#' x <- read_rds('test_data.rds') %>% filter(X5 < 0) %>% select(X5) %$% X5
#'
profit_factor <- function(x){

  # calculate profit factor
  res <- sum(x[x>0])/(0.0001+sum(abs(x[x<0])))

  # limit profit factor to maximum value of 10 (in case no losses)
  if(res > 100) res <- 10
  return(res)

  }

