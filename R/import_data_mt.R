# import data function
# (C) 2018 Vladimir Zhbanko
# -------------------------
# Import Data to R from the Sandbox
# -------------------------
# Function imports file and change data column type
# Function return the dataframe with trade data

#' Import Market Type related Data to R from the Sandbox
#'
#' @param system_number - magic number id of the trading system
#' @param path_terminal - path to the sandbox
#'
#' @return function return the data frame with 3 columns including market type code
#' @export
#'
#' @examples
import_data_mt <- function(path_terminal, system_number){
  # path_terminal <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"
  # system_number <- 8139124
  require(tidyverse)
  trade_log_file <- paste0("MarketTypeLog", system_number, ".csv")
  DFT1 <- try(read_csv(file = file.path(path_terminal, trade_log_file), 
               col_names = c("MagicNumber", "TicketNumber", "MarketType"),
               col_types = "iic"), 
      silent = TRUE)
  if(class(DFT1)[1] == "try-error") {stop("Error reading file. File with trades may not exist yet!",
                                       call. = FALSE)}
  if(!nrow(DFT1)==0){
    # data frame preparation
    DFT1$MarketType <- as.factor(DFT1$MarketType)
    # removes duplicates
    DFT1 <- unique(DFT1)
    
    return(DFT1)
  } else {
    stop("No trades executed so far. Trade data log is empty!",
         call. = FALSE)
    }

}