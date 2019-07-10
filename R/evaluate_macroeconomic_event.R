#' Function used to evaluate market type situation by reading the file with Macroeconomic Events
#'
#' @description Function is reading the content of the file 01_MacroeconomicEvent.csv.
#' Should the content of the file is 1 then Macro Economic event is present,
#' all trading robots will not be able to open new orders
#'
#' @details This function is used exclusively with Market Type recognition system
#'
#' @param setup_path Path with trading robots magic numbers setup, typically file 'Setup.csv'
#' @param file_name Name of the file
#' @param path_T1 Path of the Terminal 1
#' @param path_T3 Path of the Terminal 3
#'
#' @return Function will write files indicating to enable or disable trading systems to open new orders
#' @export
#'
#' @examples
#'
#'
#'
#' # evaluate data on macroeconomic event (required to start trading)
#' library(tidyverse)
#' #setup_path <- "/usr/local/lib/R/site-library/lazytrade/extdata/Setup.csv"
#' #file_name <- "/usr/local/lib/R/site-library/lazytrade/extdata/01_MacroeconomicEvent.csv"
#' #path_T1 <- ""
#' #path_T3 <- "/tmp/Rtmpav7Npc/file5bd8123bd8"
#' evaluate_macroeconomic_event(setup_path = system.file("extdata",
#'                                                  "Setup.csv",
#'                                                  package = "lazytrade"),
#'                              file_name = system.file("extdata",
#'                                                  "01_MacroeconomicEvent.csv",
#'                                                  package = "lazytrade"),
#'                              path_T1 = "",path_T3 = tempfile())
#'
#'
#'
#'
evaluate_macroeconomic_event <- function(setup_path, file_name, path_T1, path_T3){
  requireNamespace("tidyverse", quietly = TRUE)

  # file_name <- "01_MacroeconomicEvent.csv"
  # path_t1 <- path_T1
  # path_t3 <- path_T3

  ##========================================
  # -------------------------
  # stopping all systems when macroeconomic event is present
  # this will be covered in the Course #5 of the Lazy Trading Series!
  # -------------------------
  if(file.exists(file.path(path_T1, file_name))){
    #read the file containing a flag (1 will mean that event is present hence no new opened orders)
    DF_NT <- read_csv(file= file.path(path_T1, file_name), col_types = "i")
    #read the table of trading robots in operation
    DF_Setup <- read_csv(setup_path)

    ## condition to disable systems
    if(DF_NT[1,1] == 1) {
      # disable trades in T1
      DF_Setup %>%
        group_by(Magic) %>% select(Magic) %>% mutate(IsEnabled = 0) %>%
        # write commands to disable systems
        writeCommandViaCSV(path_T1)

      # disable trades in T3
      DF_Setup %>% group_by(Magic) %>%
        mutate(MagicNumber = Magic + 200, IsEnabled = 0) %>%
        group_by(MagicNumber) %>%
        select(MagicNumber, IsEnabled) %>%
        # write commands to disable systems
        writeCommandViaCSV(path_T3)

    }

    ## condition to enable systems
    if(DF_NT[1,1] == 0) {
      # disable trades in T1
      DF_Setup %>%
        group_by(Magic) %>% select(Magic) %>% mutate(IsEnabled = 1) %>%
        # write commands to disable systems
        writeCommandViaCSV(path_T1)

      # disable trades in T3
      DF_Setup %>% group_by(Magic) %>%
        mutate(MagicNumber = Magic + 200, IsEnabled = 1) %>%
        group_by(MagicNumber) %>%
        select(MagicNumber, IsEnabled) %>%
        # write commands to disable systems
        writeCommandViaCSV(path_T3)

    }

  }


}
