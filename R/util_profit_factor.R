#' Calculate Profit Factor
#'
#' @description Calculate profit factor using a data vector with the trading results
#'
#' `r lifecycle::badge('stable')`
#'
#' @param x column vector with profit or loss of the orders for one system
#'
#' @return function should calculate profit factor for this vector and return one value also as vector
#'
#' @export
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#' library(lazytrade)
#' data(profit_factor_data)
#' profit_factor_data %>%
#'    group_by(X1) %>%
#'    summarise(PnL = sum(X5),
#'              NumTrades = n(),
#'              PrFact = util_profit_factor(X5)) %>%
#'    select(PrFact) %>% head(1) %>% round(3)
#'
#'
util_profit_factor <- function(x){

  # calculate profit factor
  res <- sum(x[x>0])/(0.0001+sum(abs(x[x<0])))

  # limit profit factor to maximum value of 10 (in case no losses)
  if(res > 100) res <- 10
  return(res)

  }

