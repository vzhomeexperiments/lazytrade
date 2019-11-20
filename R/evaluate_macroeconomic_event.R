#' Function used to evaluate market type situation by reading the file with Macroeconomic Events
#' and writing a trigger to the trading robot
#'
#' @description Function is reading the content of the file 01_MacroeconomicEvent.csv.
#' Content of the file can be either 1 or 0.
#' 1 - when Macro Economic event is present, 0 - when it's not.
#' Function will also read magic number of the trading robots. This is indicated in the file 'Setup.csv'.
#' Final outcome of the function is the series of files written to the destination directories.
#' These files will either enable or disable opening of new positions in the trading robots
#'#'
#'
#' @details This function is used exclusively with Market Type recognition system.
#'
#' Final evaluation will consist in writing a dedicated file with a simple information:
#'
#' When Macro economic even is not present:
#'
#' "Magic","IsEnabled"
#' 8139125,1
#'
#' or, when Macro economic event is present:
#'
#' "Magic","IsEnabled"
#' 8139125,0
#'
#' @param path_T1 Path of the Terminal 1
#' @param path_T3 Path of the Terminal 3
#' @param setup_file_path string, path to the folder with Setup.csv file
#' @param setup_file_name string, name of the file 'Setup.csv'
#' @param macro_event_path string, path to the folder with a file '01_MacroeconomicEvent.csv'
#' @param macro_file_name string, name of the file '01_MacroeconomicEvent.csv'
#'
#' @return Function will write files indicating to enable or disable trading systems to open new orders
#' @export
#'
#' @examples
#'
#' # evaluate data on macroeconomic event (required to start trading)
#' library(dplyr)
#' library(readr)
#'
#' dir <- normalizePath(tempdir(),winslash = "/")
#'
#' evaluate_macroeconomic_event(setup_file_path = system.file('extdata', package = "lazytrade"),
#'                              setup_file_name = "Setup.csv",
#'                              macro_event_path = system.file('extdata', package = "lazytrade"),
#'                              macro_file_name = "01_MacroeconomicEvent.csv",
#'                              path_T1 = dir, path_T3 = dir)
#'
#'
#'
evaluate_macroeconomic_event <- function(setup_file_path,
                                         setup_file_name = "Setup.csv",
                                         macro_event_path,
                                         macro_file_name = "01_MacroeconomicEvent.csv",
                                         path_T1, path_T3){
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  # define path to the setup file
  setup_complete_path <- file.path(setup_file_path, setup_file_name)

  # define path to the macro event file
  macro_complete_path <- file.path(macro_event_path, macro_file_name)

  if(!file.exists(setup_complete_path) || !file.exists(setup_complete_path)) {
    stop("Required files do not exist", call. = FALSE)
  }

  # normalize path
  path_T1 <- normalizePath(path_T1, winslash = "/")
  path_T3 <- normalizePath(path_T3, winslash = "/")
  ##========================================
  # -------------------------
  # stopping all systems when macroeconomic event is present
  # this will be covered in the Course #5 of the Lazy Trading Series!
  # -------------------------

    #read the file containing a flag (1 will mean that event is present hence no new opened orders)
    DF_NT <- read_csv(file= macro_complete_path, col_types = "i")
    #read the table of trading robots in operation
    DF_Setup <- read_csv(setup_complete_path)

    ## condition to disable systems
    if(DF_NT[1,1] == 1) {
      # disable trades in T1
      DF_Setup %>%
        group_by(Magic) %>% select(Magic) %>% mutate(IsEnabled = 0) %>%
        # write commands to disable systems
        write_command_via_csv(path_T1)

      # disable trades in T3
      DF_Setup %>% group_by(Magic) %>%
        mutate(MagicNumber = Magic + 200, IsEnabled = 0) %>%
        group_by(MagicNumber) %>%
        select(MagicNumber, IsEnabled) %>%
        # write commands to disable systems
        write_command_via_csv(path_T3)

    }

    ## condition to enable systems
    if(DF_NT[1,1] == 0) {
      # enable trades in T1
      DF_Setup %>%
        group_by(Magic) %>% select(Magic) %>% mutate(IsEnabled = 1) %>%
        # write commands to disable systems
        write_command_via_csv(path_T1)

      # enable trades in T3
      DF_Setup %>% group_by(Magic) %>%
        mutate(MagicNumber = Magic + 200, IsEnabled = 1) %>%
        group_by(MagicNumber) %>%
        select(MagicNumber, IsEnabled) %>%
        # write commands to disable systems
        write_command_via_csv(path_T3)

    }

  }



