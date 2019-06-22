#' Table with Trade States and sample of actual policy for those states
#'
#' @docType data
#' @keywords datasets
#'
#' @format A dataframe with 2 columns:
#' \describe{
#' \itemize{
#'   \item{TradeState}{Current trade state status}
#'   \item{Policy}{Policy choice}
#' }
#' }
"TradeStatePolicy"

#' Table with Trade results samples
#'
#' @docType data
#' @keywords datasets
#'
#' @format A dataframe with several columns:
#' \describe{
#' \itemize{
#'   \item{MagicNumber}{Unique identifiers of the Trading Robots}
#'   \item{TicketNumber}{Ticket Number of closed position}
#'   \item{OrderStartTime}{Date and Time when order started}
#'   \item{OrderCloseTime}{Date and Time when order closed}
#'   \item{Profit}{Monetary result of the trade}
#'   \item{Symbol}{Symbol of the Asset e.g. EURUSD}
#'   \item{OrderType}{Order Type 0 - buy, 1 - sell}
#' }
#' }
"profit_factorDF"

#' Table with Trade results samples
#'
#' @docType data
#' @keywords datasets
#'
#' @format A dataframe with several columns:
#' \describe{
#' \itemize{
#'   \item{X1}{Unique identifiers of the Trading Robots}
#'   \item{X2}{Ticket Number of closed position}
#'   \item{X3}{Date and Time when order started}
#'   \item{X4}{Date and Time when order closed}
#'   \item{X5}{Monetary result of the trade}
#'   \item{X6}{Symbol of the Asset e.g. EURUSD}
#'   \item{X7}{Order Type 0 - buy, 1 - sell}
#' }
#' }
"profit_factor_data"

#' Table with Trade results samples
#'
#' @docType data
#' @keywords datasets
#'
#' @format A dataframe with several columns:
#' \describe{
#' \itemize{
#'   \item{MagicNumber}{Unique identifiers of the Trading Robots}
#'   \item{TicketNumber}{Ticket Number of closed position}
#'   \item{OrderStartTime}{Date and Time when order started}
#'   \item{OrderCloseTime}{Date and Time when order closed}
#'   \item{Profit}{Monetary result of the trade}
#'   \item{Symbol}{Symbol of the Asset e.g. EURUSD}
#'   \item{OrderType}{Order Type 0 - buy, 1 - sell}
#' }
#' }
"data_trades"
