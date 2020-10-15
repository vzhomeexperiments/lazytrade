#' Function that returns the profit factors of the systems in a form of a DataFrame
#'
#'
#'
#' @param x - data frame with orders. Note x must contain MagicNumber and Profit columns!
#' @param num_orders - desired number of orders to base profit factor calculation
#'
#' @return - Function returns dataframe with column PrFact with calculated profit factor value for each trading robot
#' @export
#'
#' @examples
#'
#' library(lazytrade)
#' library(dplyr)
#' library(magrittr)
#' data(profit_factorDF)
#' get_profit_factorDF(x = profit_factorDF,
#'                     num_orders = 10)
#'
#'
#'
get_profit_factorDF <- function(x, num_orders){
  requireNamespace("dplyr", quietly = TRUE)
  # generate DF with only MagicNumbers when > 10 trades and all trades are losers

  x <- x %>% ungroup() #ungroup to remove warning

  DF_L <- x %>%
    dplyr::group_by(MagicNumber) %>%
    dplyr::summarise(nOrders = n(), .groups = "drop_last") %>%

    dplyr::filter(nOrders > num_orders) %>%
    dplyr::select(MagicNumber) %>%
    # subset only rows that contans magic numbers from x
    dplyr::inner_join(x, by = "MagicNumber") %>%
    dplyr::group_by(MagicNumber) %>%
    dplyr::filter(Profit < 0) %>%
    dplyr::summarise(Loss = abs(sum(Profit)), .groups = "drop_last")
  # generate DF with only MagicNumbers when > 10 trades and all trades are profits
  DF_P <- x %>%
    dplyr::group_by(MagicNumber) %>%
    dplyr::summarise(nOrders = n(), .groups = "drop_last") %>%
    dplyr::filter(nOrders > num_orders)%>%
    dplyr::select(MagicNumber)%>%
    # subset only rows that contans magic numbers from x
    dplyr::inner_join(x, by = "MagicNumber")%>%
    dplyr::group_by(MagicNumber)%>%
    dplyr::filter(Profit > 0) %>%
    dplyr::summarise(Gain = abs(sum(Profit)), .groups = "drop_last")
  # generate DF containing profit factor of all systems
  DF <- DF_P %>%
    dplyr::full_join(DF_L, by = "MagicNumber")
  # replace any NA with 1!
  DF[is.na(DF)] <- 1
  # calculate profit factors of the each system!
  DF_PF <- DF%>%
    dplyr::group_by(MagicNumber)%>%
    dplyr::mutate(PrFact = Gain/(0.001+Loss))%>%
    dplyr::select(MagicNumber, PrFact)
  return(DF_PF)
}
