#' Write csv files with indicated commands to the external system
#'
#' @description Function is capable to read the data and  writing multiple files e.g. 'SystemControl8139124.csv'
#'
#' @param x - dataframe object with resulting command e.g. 1 - enable; 0 - disable
#' @param path_terminal - path to the terminal
#' @param fileName - desired control file prefix e.g. 'SystemControl'
#'
#' @return Function is writing multiple files e.g. 'SystemControl8139124.csv' to the Sandbox
#'
#' typical content of the file:
#' "Magic","IsEnabled"
#' 8139124,1
#'
#' @export
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @examples
#'
#' library(dplyr)
#' library(readr)
#' library(lubridate)
#' library(lazytrade)
#'
#' path_sbxm <- normalizePath(tempdir(),winslash = "/")
#'
#' file.copy(from = system.file("extdata", "OrdersResultsT1.csv", package = "lazytrade"),
#'           to = file.path(path_sbxm, "OrdersResultsT1.csv"), overwrite = TRUE)
#'
#' DFT1 <- import_data(path_sbxm = path_sbxm,
#'                     trade_log_file = "OrdersResultsT1.csv")
#'
#' DFT1 %>%
#' group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0) %>%
#' # head to shorten time of this example
#' head(2) %>%
#' # write commands to disable/enable systems
#' write_command_via_csv(path_terminal = path_sbxm)
#'
#'
#'
#'
write_command_via_csv <- function(x, path_terminal = "", fileName = "SystemControl"){
  # check if the provided object 'x' exists and that it's not empty
  if(exists("x") == TRUE && nrow(x) != 0) {
    # for loop to go through the object x
    for(i in 1:nrow(x))
    {
      # write the file for MQL4 usage
      composed_name <- paste0(fileName, as.character(x[i, 1]), ".csv")
      f_name <- file.path(path_terminal, composed_name)
      write.csv(x[i, ], file = f_name, row.names = FALSE)

    }
  }
}
