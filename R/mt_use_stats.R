#' Function to use statistical method for Market Type recognition
#'
#' @description  Function is using log return distribution of financial asset's price.
#' Main idea is to estimate Market Type by solely relying on the current price pattern.
#' Latest price is obtained from the file, typically AI_CP60-300.csv. Log returns are
#' being calculated for each column
#'
#' Selected Market Periods according to the theory from Van K. Tharp:
#' 1. Bull normal, BUN
#' 2. Bull volatile, BUV
#' 3. Bear normal, BEN
#' 4. Bear volatile, BEV
#' 5. Sideways quiet, RAN
#' 6. Sideways volatile, RAV
#'
#' @details Function is using arbitrary defined levels of distributions.
#'
#' @author (C) 2020 Vladimir Zhbanko
#' @backref Market Type research of Van Tharp Institute: <https://www.vantharp.com/>
#'
#' @param indicator_dataset   Dataframe, One column containing indicator patterns to train the model
#' @param symbol              Character symbol of the asset
#' @param num_bars            Integer, Number of bars used to detect pattern
#' @param timeframe           Integer, Data timeframe in Minutes.
#' @param path_data           String, User path where the latest price data is stored
#' @param path_sbxm           String, User path to the sandbox where file with strategy test results should be written (master terminal)
#' @param path_sbxs           String, User path to the sandbox where file with strategy test results should be written (slave terminal)
#'
#' @return Log returns dataframe
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' library(dplyr)
#' library(magrittr)
#' library(readr)
#' library(lazytrade)
#' library(ggplot2)
#' library(lubridate)
#'
#' path_data <- normalizePath(tempdir(),winslash = "/")
#' path_sbxm <- normalizePath(tempdir(),winslash = "/")
#' path_sbxs <- normalizePath(tempdir(),winslash = "/")
#'
#' ind = system.file("extdata", "AI_CP60-300.csv",
#'                   package = "lazytrade") %>% read_csv(col_names = FALSE)
#'
#' ind$X1 <- ymd_hms(ind$X1)
#' tick = system.file("extdata", "TickSize_AI_RSIADX.csv",
#'                   package = "lazytrade") %>% read_csv(col_names = FALSE)
#'
#' write_csv(ind, file.path(path_data, "AI_CP60-300.csv"), col_names = FALSE)
#' write_csv(tick, file.path(path_data, "TickSize_AI_RSIADX.csv"), col_names = FALSE)
#'
#' ind1 <- select(ind, X2)
#' #'
#' # performing Deep Learning Regression using the custom function
#' mt_use_stats(indicator_dataset = ind1,
#'              symbol = 'EURUSD',
#'              num_bars = 64,
#'              timeframe = 60,
#'              path_data = path_data,
#'              path_sbxm = path_sbxm,
#'              path_sbxs = path_sbxs)
#'
#'
#' #set delay to insure h2o unit closes properly before the next test
#' Sys.sleep(5)
#'
#' }
#'
#'
#'
mt_use_stats <- function(indicator_dataset,
                         symbol,
                         num_bars=64,
                         timeframe = 60,
                         path_data,
                         path_sbxm,
                         path_sbxs){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

  #construct the path to the data objects
  # generate a file name to be able to read the right dataset
  f_name <- paste0("AI_CP", timeframe, "-300.csv")
  full_path <- file.path(path_data,  f_name)
  # file name with the tick data
  path_tick <- file.path(path_data, "TickSize_AI_RSIADX.csv")

  #dataset with tick data
  z <- readr::read_csv(path_tick, col_names = FALSE) %>%
    #filter line with a symbol we need
    dplyr::filter(X1 == symbol) %$%
    #value z will contain tick value for this symbol
    X2

sym_ret <- ((indicator_dataset[2:num_bars,1] - indicator_dataset[1:(num_bars-1), 1])/
               indicator_dataset[1:(num_bars-1), 1])/z


log_ret <- (log(indicator_dataset[2:num_bars,1])-log(indicator_dataset[1:(num_bars-1), 1]))/z

#sum(log_ret$USDJPY)
Q1 <- quantile(log_ret[,1], 0.25)
Q2 <- quantile(log_ret[,1], 0.5)
Q3 <- quantile(log_ret[,1], 0.75)

# # kurtosis with package 'moments'
# moments::kurtosis(log_ret)
# moments::skewness(log_ret)
# moments::moment(log_ret)


# # arbitrary decision
# if (Q2 > 0 && Q1 > -15 && Q3 > 15) {
#   return('BUN')
# } else if(Q2 > 0 && Q1 > -5 && Q3 > 30) {
#   return('BUV')
# } else if(Q2 < 0 && Q1 > -15 && Q3 > 5) {
#   return('BEN')
# } else if(Q2 < 0 && Q1 > -35 && Q3 > 15) {
#   return('BEV')
# } else if(Q2 = 0 && Q1 > -5 && Q3 > 5) {
#   return('RAN')
# } else if(Q2 = 0 && Q1 > -30 && Q3 > 30) {
#   return('RAV')
# }

return(log_ret)
#dplyr::mutate(log_ret, qntile = quantile(symbol, 0.5)) %>% head(1) %>% dplyr::pull(qntile)
# ggplot2::ggplot(ind, ggplot2::aes(x = X1, y = X29))+ggplot2::geom_line()
#ggplot2::ggplot(sym_ret, ggplot2::aes(x = sym_ret[,1]))+ggplot2::geom_density()


}

