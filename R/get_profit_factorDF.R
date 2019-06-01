#
#' Function that returns the profit factors of the systems in a form of DataFrame
#' 
#' 
#'  
#' @param x - data frame with orders. Note x must contain MagicNumber and Profit columns!
#' @param num_orders - desired number of orders to base profit factor calculation
#'
#' @return - Function return dataframe with column PrFact with calculated profit factor value for each trading robot
#' @export
#'
#' @examples
get_profit_factorDF <- function(x, num_orders){
  # generate DF with only MagicNumbers when > 10 trades and all trades are losers
  # x <- read_rds("_TEST_DATA/test_data_profit_factorDF.rds")
  # num_orders <- 10
  DF_L <- x %>%
    group_by(MagicNumber) %>%
    summarise(nOrders = n())%>%
    filter(nOrders > num_orders)%>%
    select(MagicNumber)%>%
    # subset only rows that contans magic numbers from x
    inner_join(x, by = "MagicNumber")%>%
    group_by(MagicNumber)%>%
    filter(Profit < 0) %>%
    summarise(Loss = abs(sum(Profit)))
  # generate DF with only MagicNumbers when > 10 trades and all trades are profits
  DF_P <- x %>%
    group_by(MagicNumber) %>%
    summarise(nOrders = n())%>%
    filter(nOrders > num_orders)%>%
    select(MagicNumber)%>%
    # subset only rows that contans magic numbers from x
    inner_join(x, by = "MagicNumber")%>%
    group_by(MagicNumber)%>%
    filter(Profit > 0) %>%
    summarise(Gain = abs(sum(Profit)))
  # generate DF containing profit factor of all systems
  DF <- DF_P %>%
    full_join(DF_L, by = "MagicNumber")
  # replace any NA with 1!
  DF[is.na(DF)] <- 1
  # calculate profit factors of the each system!
  DF_PF <- DF%>%
    group_by(MagicNumber)%>%
    mutate(PrFact = Gain/(0.001+Loss))%>%
    select(MagicNumber, PrFact)
  return(DF_PF)
}
