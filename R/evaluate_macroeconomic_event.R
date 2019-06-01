#' Function used to evaluate market type situation by reading the file. It will enable or disable trading robots
#' accordingly. To be used exclusively with Market Type recognition system
#'
#' @param setup_path Path with trading robots magic numbers
#' @param file_name Name of the file
#' @param path_t1 Path of the Terminal 1
#' @param path_t3 Path of the Terminal 2
#'
#' @return
#' @export
#'
#' @examples
evaluate_macroeconomic_event <- function(setup_path, file_name, path_t1, path_t3){
  
  # setup_path <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_F2/TEST/Setup.csv"
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