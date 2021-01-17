#' Function to read new data, transform, aggregate and save data for further retraining of regression model
#' for a single currency pair
#'
#' @description  Function is collecting data from the csv files
#' Data objects are transformed to be suitable for Regression Modelling.
#' Price change will be in the column 'LABEL', column X1 will keep the time index
#' Result will be written to a new or aggregated to the existing '.rds' file
#'
#' Function is keeping generated dataset to be not larger than specified by the user
#'
#' @details Function is not handling shift of the price and indicator datasets.
#'
#' This function is relying on the data collection from the dedicated data robot
#' Other 'aml_*' functions will work based on the data processed by this function
#'
#' @author (C) 2020 Vladimir Zhbanko
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
#' # data transformation using the custom function for one symbol
#' aml_collect_data(indicator_dataset = ind,
#'                  symbol = 'USDJPY',
#'                  timeframe = 60,
#'                  path_data = path_data)
#'
#'
aml_collect_data <- function(indicator_dataset, symbol, timeframe, path_data,
                             max_nrows = 2500){

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)

  # create a new column 'LABEL'
  dat11 <- indicator_dataset %>%
    # find the price difference between now and xx bars ago also consider JPY pairs...
    dplyr::mutate(LABEL = ifelse(X2 < 10, 10000*(X3-X2), 100 * (X3-X2)))

  # dataset lagging will be performed before modelling
  # dat12 <- dat11 %>%
  #   # lagging the dataset:    %>% mutate_all(~lag(., n = 28))
  #   dplyr::mutate(dplyr::across(LABEL, ~lag(., n = 34))) %>%
  #   # remove empty rows
  #   na.omit() %>%
  #    # Note: Zero values in rows will mean that there was no data in the MT4 database.
  #   filter_all(any_vars(. != 0))

  # checking the data: summary(dat11) # too high values in the LABEL Column are non-sense! hist(dat11$LABEL)

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

