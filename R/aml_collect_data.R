#' Function to read new data, transform data, save data for further retraining of regression model for a single currency pair
#'
#' @description  Function is collecting data from the files using dedicated function load_asset_data.R.
#' One file with a prices of the asset and another file with the corresponding indicator pattern.
#' Both data objects are transformed to be siutable for Regression Modelling.
#' Indicator values will be placed into the column X1-X75 and price change is in the column 'LABEL'
#' Result would be written to new or aggregated to the existing file
#'
#' Function is also checking that generated dataset is not too big.
#' Should the dataset is too big (e.g. > 1000000 rows), then only latest 950000 rows will be used.
#' Note: the amount 1000000 rows is not verified in practice, further testing is required.
#'
#' @details Function is handling shift of the price and indicator datasets.
#' New data will be always on the 'bottom' of the dataset
#'
#' The amount of rows is customizable however it must be selected once for the function to start working.
#' Other 'aml_*' functions will rely on this selections, use the same number accordingly!
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @param price_dataset       Dataset containing assets prices. It will be used as a label
#' @param indicator_dataset   Dataset containing assets indicator which pattern will be used as predictor
#' @param symbol              Character symbol of the asset for which to train the model
#' @param num_bars            Number of bars used to detect pattern
#' @param timeframe           Data timeframe e.g. 1 min
#' @param path_data           Path where the aggregated historical data is stored, if exists in rds format
#'
#' @return Function is writing files into Decision Support System folder, mainly file object with the model
#' @export
#'
#' @examples
#'
#'
#'
#' # write examples for the function
#' library(tidyverse)
#' library(lubridate)
#' library(lazytrade)
#'
#' path_terminal <- system.file("extdata", package = "lazytrade")
#' macd <- load_asset_data(path_terminal = path_terminal, trade_log_file = "AI_Macd",
#'                         time_period = 15, data_deepth = "300")
#'
#' prices <- load_asset_data(path_terminal = path_terminal, trade_log_file = "AI_CP",
#'                           time_period = 15, data_deepth = "300")
#'
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#'
#' # data transformation using the custom function for one symbol
#' aml_collect_data(price_dataset = prices,
#'                  indicator_dataset = macd,
#'                  symbol = 'EURUSD',
#'                  num_bars = 75,
#'                  timeframe = 15,
#'                  path_data = path_data)
#'
#'
#'
#'
aml_collect_data <- function(price_dataset, indicator_dataset, symbol, num_bars, timeframe, path_data){

  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)

  # add mapping of columns to the symbols
  # Vector of currency pairs
  Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
            "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
            "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
            "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")
  # find the index of this pair in the vector of columns
  cn <- which(symbol %in% Pairs) + 1

  # only have data for one symbol
  price_dataset <- price_dataset[, c(1,cn)]
  indicator_dataset <- indicator_dataset[, c(1,cn)]
  # transform data and get the labels shift rows down Note: the oldest data in the first row!!
  dat14 <- create_labelled_data(price_dataset, num_bars, type = "regression")
  dat14 <- rbind(NA, dat14[-nrow(dat14), ])

  # transform data for indicator. Note: the oldest data in the first row!!
  dat15 <- create_transposed_data(indicator_dataset, num_bars)
  # dataframe for the DL modelling it contains all the available data.
  # Note: Zero values in rows will mean that there was no data in the MT4 database.
  #       These rows will be removed before modelling however it's advisable not to have those as it might give data artefacts!
  dat16 <- dat14 %>% select(LABEL) %>% bind_cols(dat15) %>% na.omit() %>% filter_all(any_vars(. != 0)) %>% filter(LABEL < 600, LABEL > -600)
  # checking the data: summary(dat16) # too high values in the LABEL Column are non-sense! hist(dat16$LABEL)
  # add better names
  col_n1 <- "LABEL"
  col_n2 <- c(paste("X",1:num_bars,sep=""))
  col_add <- c(col_n1, col_n2)
  df_row <- dat16 %>% head(1)
  colnames(df_row) <- col_add

  ## ---------- Data Saving ---------------

  # generate a file name
  f_name <- paste0(symbol, "M",timeframe,"X",num_bars, ".rds")
  full_path <- file.path(path_data,  f_name)

  # check that old data in the file name is exist or not...

  # retrieve already recorded data >> add temporary dataframe >> write to the data_update folder
  # check if there is a rds file in the data_update folder and add the df_temp there
  if(exists("df_row") && !file.exists(full_path))
  {
    # write file first time
    write_rds(df_row, full_path)
  } else if(exists("df_row") && file.exists(full_path)) {
    # read previous file
    read_rds(full_path) %>%
      # join obtained data below! existing one
      bind_rows(df_row) %>%
      # write data back
      write_rds(full_path)
    #verify generated data
    # x1 <- read_rds(full_path)
  }

  # add module of code that limit the data amount (e.g. delete too old data, leave max 1 mln(?) rows)
  # ---
  # check number of rows
  x1_nrows <- read_rds(full_path) %>% nrow()
  # what to do if too much rows?
  if(x1_nrows > 1000000){
    # read all the data
    read_rds(full_path) %>%
      # use only last 950000 rows, 950000 is to avoid this code to run so often...
      tail(950000) %>%
      # write them back
      write_rds(full_path)
  }

  # ---


}

