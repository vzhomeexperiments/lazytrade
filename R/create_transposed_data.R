## FUNCTION create_transposed_data
## PURPOSE: function gets indicator data in each column
## it is splitting this data by periods and transpose the data. 
## additionally it is label the data based on the simple logic assigning it to 2 categories based on the difference
## between beginning and end of the vector
## finally it is stacking all data and joining everything into the table 

## TEST:
# library(tidyverse)
# library(lubridate)
# pathT2 <- "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files/"
# macd <- read_csv(file.path(pathT2, "AI_Macd1.csv"), col_names = F)
# macd$X1 <- ymd_hms(macd$X1)
# write_rds(macd, "test_data/macd.rds")

#' Create Transposed Data
#' https://www.udemy.com/self-learning-trading-robot/?couponCode=LAZYTRADE7-10
#'
#' @param x - data set containing a table where 1st column is a Time index and other columns containing financial asset indicator values
#' @param n - number of rows we intend to split and transpose the data 
#'
#' @return function returns transposed data. Transposed values from every column are stacked one to each other
#' @export
#'
#' @examples
#' 
create_transposed_data <- function(x, n = 50){
  require(tidyverse)
  #source("C:/Users/fxtrams/Documents/000_TradingRepo/R_selflearning/_FUN/load_data.R")
  #n <- 100
  #x <- load_data(path_terminal = "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files/", trade_log_file = "AI_Macd", time_period = 1, data_deepth = "50000")
  #x <- read_rds("_TEST_DATA/indicator_dataset.rds")
  nr <- nrow(x)
  dat11 <- x %>% select(-1) %>% split(rep(1:ceiling(nr/n), each=n, length.out=nr)) #list
  dat11[length(dat11)] <- NULL
  
  # operations within the list
  for (i in 1:length(dat11)) {
    #i <- 1
    
    if(!exists("dfr12")){
      dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% as_tibble(.name_repair = "universal", verbose =F) } else {
        dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% as_tibble(.name_repair = "universal", verbose =F) %>% bind_rows(dfr12)
      }
    
  }
  
  return(dfr12)
  
}

