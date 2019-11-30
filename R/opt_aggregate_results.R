#' Function to aggregate trading results from multiple folders and files
#'
#' @description PURPOSE: Read multiple files stored in different folders
#' Store results to the intermediate dataframe.
#'
#' @details user must provide the path to the files in the folders
#' all files in subfolders are read and aggregated into one data object.
#' Data object is sorted in descending order by order close time
#'
#' @param fold_path - path to the folder containing subfolders
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
#'  DFOLDER <- system.file("extdata/RES", package = "lazytrade")
#'  #dir <- normalizePath(tempdir(),winslash = "/")
#'  opt_aggregate_results(fold_path = DFOLDER)
#'
#'
opt_aggregate_results <- function(fold_path){

  # folders with results
    # join paths into a vector
  DFOLDER <- dir(fold_path, full.names = TRUE)


  for (FOLDER in DFOLDER) {
    # FOLDER <- DFOLDER[2]

    filesToRead <-list.files(FOLDER, pattern="*.csv", full.names=F)
    #error management (if empty folder)
    if(length(filesToRead) == 0) {warning("Empty Folder!",call. = FALSE)
                                  next()}

    for (FILE in filesToRead) {
      #FILE <- filesToRead[2]
      # import data
      DF_TEST <- import_data(path_terminal = FOLDER, trade_log_file = FILE)
      # convert factor to character
      DF_TEST$Symbol <- as.character(DF_TEST$Symbol)
      DF_TEST$OrderType <- as.character(DF_TEST$OrderType)
      #agregate
      if (!exists("DF_TEMP")) {
        DF_TEMP <- DF_TEST
      } else {
        DF_TEMP <- DF_TEMP %>% bind_rows(DF_TEST)
      }

    }

    #agregate
    if (!exists("DF_TEMP1")) {
      DF_TEMP1 <- DF_TEMP
    } else {
      DF_TEMP1 <- DF_TEMP1 %>% bind_rows(DF_TEMP)
    }

    # remove agregated results from the first folder
    rm(DF_TEMP)

  }

  DFR <- DF_TEMP1 %>%
    #sort by close date
    arrange(OrderCloseTime) %>%
    #create cumulative sum column
    mutate(CUMSUM_PNL = cumsum(Profit))

  return(DFR)

}
