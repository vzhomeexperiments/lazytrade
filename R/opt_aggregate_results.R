#' Function to aggregate trading results from multiple folders and files
#'
#' @description Read multiple '.csv' files stored in different folders
#' Store results to the intermediate dataframe.
#'
#' `r lifecycle::badge('deprecated')`
#'
#' @details user must provide the path to the files in the folders
#' all files in subfolders are read and aggregated into one data object.
#' Data object is sorted in descending order by order close time
#'
#' @param path_data - String, path to the folder containing subfolders
#'
#' @return Dataframe with trading results
#'
#' @export
#'
#' @examples
#'
#'  library(lazytrade)
#'  library(readr)
#'  library(dplyr)
#'  library(magrittr)
#'  library(lubridate)
#'
#'  dir <- normalizePath(tempdir(),winslash = "/")
#'
#'  file.copy(from = system.file("extdata/RES", package = "lazytrade"),
#'            to = dir, recursive = TRUE)
#'
#'  DF_RES <- opt_aggregate_results(path_data = file.path(dir, "RES"))
#'
#'
opt_aggregate_results <- function(path_data){


  lifecycle::deprecate_warn(when = "0.5.1",
                            what = "opt_aggregate_results()",
                            details = "This function is not used."
  )

  # folders with results
    # join paths into a vector
  DFOLDER <- dir(path_data, full.names = TRUE)

  #list.files(path = DFOLDER, pattern = ".csv", recursive = TRUE)

  for (FOLDER in DFOLDER) {
    # FOLDER <- DFOLDER[2]

    filesToRead <-list.files(FOLDER, pattern=".csv", full.names=FALSE, recursive = TRUE)
    #error management (if empty folder)
    if(length(filesToRead) == 0) {warning("Empty Folder!",call. = FALSE)
                                  next()}

    for (FILE in filesToRead) {
      #FILE <- filesToRead[2]
      # import data
      DF_TEST <- lazytrade::import_data(path_sbxm = FOLDER, trade_log_file = FILE)
      # convert factor to character
      DF_TEST$Symbol <- as.character(DF_TEST$Symbol)
      DF_TEST$OrderType <- as.character(DF_TEST$OrderType)
      #agregate
      if (!exists("DF_TEMP")) {
        DF_TEMP <- DF_TEST
      } else {
        DF_TEMP <- DF_TEMP %>% dplyr::bind_rows(DF_TEST)
      }

    }

    #agregate
    if (!exists("DF_TEMP1")) {
      DF_TEMP1 <- DF_TEMP
    } else {
      DF_TEMP1 <- DF_TEMP1 %>% dplyr::bind_rows(DF_TEMP)
    }

    # remove agregated results from the first folder
    rm(DF_TEMP)

  }

  DFR <- DF_TEMP1 %>%
    #sort by close date
    dplyr::arrange(OrderCloseTime) %>%
    #create cumulative sum column
    dplyr::mutate(CUMSUM_PNL = cumsum(Profit))

  return(DFR)

}
