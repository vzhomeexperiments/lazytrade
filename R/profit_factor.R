#' Calculate Profit Factor
#'
#' @description
#'
#' Function is capable to calculate profit factor using a vector of the trading results
#'
#' @param x column vector with profit or loss of the orders for one system
#
#' @usage
#'
#' # sumarizing table
#'  DF_Stats <- DF_Stats %>%
#'     filter(X3 > as.POSIXct(input$filterDate)) %>%
#'     group_by(X1) %>%
#'     summarise(PnL = sum(X5),
#'               NumTrades = n(),
#'               PrFact = profit_factor(X5))
#'
#' @return function should calculate profit factor for this vector and return one value also as vector
#'
#' @export
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @examples
#'
#'
#' data(profit_factor_data)
#' profit_factor_data %>%
#'    group_by(X1) %>%
#'    summarise(PnL = sum(X5),
#'              NumTrades = n(),
#'              PrFact = profit_factor(X5)) %>%
#'    select(PrFact) %>% head(1) %>% as.vector() %>% round(3)
#'
#'
profit_factor <- function(x){

  # calculate profit factor
  res <- sum(x[x>0])/(0.0001+sum(abs(x[x<0])))

  # limit profit factor to maximum value of 10 (in case no losses)
  if(res > 100) res <- 10
  return(res)

  }

