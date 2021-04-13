#' Function to read, transform, aggregate and save data for further retraining
#' of regression model for a single asset
#'
#' @description  Function is collecting data from the csv files
#' Data objects are transformed to be suitable for Regression Modelling.
#' Price change will be in the column 'LABEL', column X1 will keep the time index
#' Result will be written to a new or aggregated to the existing '.rds' file
#'
#' Function is keeping generated dataset to be not larger than specified by the user
#'
#' `r lifecycle::badge('stable')`
#'
#' @details Function is not handling shift of the price and indicator datasets.
#'
#' This function is relying on the data collection from the dedicated data robot
#' Other 'aml_*' functions will work based on the data processed by this function
#'
#' @author (C) 2020, 2021 Vladimir Zhbanko
#'
#' @param indicator_dataset   Dataset containing assets indicator which pattern will be used as predictor
#' @param symbol              Character symbol of the asset for which to train the model
#' @param timeframe           Data timeframe e.g. 1 min
#' @param path_data           Path where the aggregated historical data is stored, if exists in rds format
#' @param max_nrows           Integer, Maximum number of rows to collect
#'
#' @return Function is writing files into Decision Support System folder, mainly file object with the model
#' @export
#'
#' @examples
#'
#' # write examples for the function
#' library(dplyr)
#' library(readr)
#' library(lubridate)
#' library(lazytrade)
#' library(magrittr)
#'
#' # sample dataset
#' ind = system.file("extdata", "AI_RSIADXUSDJPY60.csv",
#'                   package = "lazytrade") %>% read_csv(col_names = FALSE)
#'
#' # convert to POSIX format
#' ind$X1 <- ymd_hms(ind$X1)
#'
#' # create temporary path (check output of tempdir() to check the result)
#' path_data <- normalizePath(tempdir(),winslash = "/")
#'
#' # add tick data to the folder
#' tick = system.file("extdata", "TickSize_AI_RSIADX.csv",
#'                   package = "lazytrade") %>% read_csv(col_names = FALSE)
#'
#' write_csv(tick, file.path(path_data, "TickSize_AI_RSIADX.csv"), col_names = FALSE)
#'
#'
#' # data transformation using the custom function for one symbol
#' aml_collect_data(indicator_dataset = ind,
#'                  symbol = 'USDJPY',
#'                  timeframe = 60,
#'                  path_data = path_data)
#'
#'
aml_collect_data <- function(indicator_dataset, symbol,
                             timeframe = 60,
                             path_data,
                             max_nrows = 2500){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)

  # fail safe check of the indicator data set
  if((sum(indicator_dataset[1:10, 2])==sum(indicator_dataset[1:10, 3]))&&
     (mean(sum(indicator_dataset[1:10, 2]))==mean(sum(indicator_dataset[1:10, 3])))){
    stop("Something is wrong in the provided input data, please check!",
         call. = FALSE)
  }

  # read tick value to calculate LABEL in pips
  # file name with the tick data
  path_tick <- file.path(path_data, "TickSize_AI_RSIADX.csv")
  # fail safe check if file is available
  if(!file.exists(path_tick)){
    stop("File with tick size data is not exist, add this file to path_data",
         call. = FALSE)
  }
  #dataset with tick data
  z <- readr::read_csv(path_tick, col_names = FALSE) %>%
    #filter line with a symbol we need
    dplyr::filter(X1 == symbol) %$%
    #value z will contain tick value for this symbol
    X2

  # create a new column 'LABEL'
  dat11 <- indicator_dataset %>%
    # find the price difference between now and xx bars ago also consider tick size
    dplyr::mutate(LABEL = (X3-X2)/(10*z))

  # dataset lagging will be performed before modelling
  # dat12 <- dat11 %>%
  #   # lagging the dataset:    %>% mutate_all(~lag(., n = 28))
  #   dplyr::mutate(dplyr::across(LABEL, ~lag(., n = 34))) %>%
  #   # remove empty rows
  #   na.omit() %>%
  #    # Note: Zero values in rows will mean that there was no data in the MT4 database.
  #   filter_all(any_vars(. != 0))

  # checking the data: summary(dat11) # too high values in the LABEL Column are non-sense! hist(dat11$LABEL)
  if(max(abs(dat11$LABEL))>500){
    warning("Calculated LABEL column values may be too high/low", call. = FALSE)
  }
  ## ---------- Data Saving ---------------

  # generate a file name
  f_name <- paste0("AI_RSIADX", symbol,timeframe, ".rds")
  full_path <- file.path(path_data,  f_name)

  # check that old data in the file name is exist or not...

  # retrieve already recorded data >> add only the new data >> write to the data folder
  # check if there is a rds file in the data folder
  if(exists("dat11") && !file.exists(full_path))
  {
    # write file first time
    readr::write_rds(dat11, full_path)
  } else if(exists("dat11") && file.exists(full_path)) {
    # read previous file and aggregate
    readr::read_rds(full_path) %>%
      # join obtained data below! existing one
      dplyr::bind_rows(dat11) %>%
      # check that data does not have double rows that are exactly same...
      dplyr::distinct() %>%
      # arrange by date in a descending order
      dplyr::arrange(desc(X1)) %>%
      # use only last N rows, that is to avoid this code to run so often...
      utils::head(max_nrows) %>%
      # write data back
      readr::write_rds(full_path)
    #verify generated data
    # x1 <- read_rds(full_path)
  }


  # ---


}

