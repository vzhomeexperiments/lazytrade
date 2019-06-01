## FUNCTION create_labelled_data
## PURPOSE: function gets price data in each column
## it is splitting this data by periods and transposes the data. 
## additionally it is label the data based on the simple logic assigning it to 2 categories based on the difference
## between beginning and end of the vector
## finally it is stacking all data and joining everything into the table 

## TEST:
# library(tidyverse)
# library(lubridate)
# pathT2 <- "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files/"
# prices <- read_csv(file.path(pathT2, "AI_CP1.csv"), col_names = F)
# prices$X1 <- ymd_hms(prices$X1)
# write_rds(prices, "test_data/prices.rds")

#' Create labelled data
#' https://www.udemy.com/self-learning-trading-robot/?couponCode=LAZYTRADE7-10
#'
#' @param x - data set containing a table where 1st column is a Time index and other columns containing financial asset price values
#' @param n - number of rows we intend to split and transpose the data 
#' @param type - type of the label required. Can be either "classification" or "regression". "classification" will return either "BU" or "BE",
#' "regression" will return the difference between first value and the last value in each row
#'
#' @return function returns transposed data. One column called 'LABEL' indicate achieved value of the label.
#' Transposed values from every column are stacked one to each other
#' 
#' @export
#'
#' @examples
create_labelled_data <- function(x, n = 50, type = "regression"){
  require(tidyverse)
  #source("C:/Users/fxtrams/Documents/000_TradingRepo/R_selflearning/_FUN/load_data.R")
  #x <- load_data(path_terminal = "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files/", trade_log_file = "AI_CP", time_period = 1, data_deepth = "50000")
  #x <- read_rds("_TEST_DATA/price_dataset.rds")
  #n <- 100
  #type <- "classification"
  #type <- "regression"
  #
  nr <- nrow(x)
  dat11 <- x %>% 
    # remove column 1 with data and time information
    select(-1) %>% 
    # split dataset into several objects each containing n rows (it will be a list)
    split(rep(1:ceiling(nr/n), each=n, length.out=nr)) #list
  # remove last element of the list
  dat11[length(dat11)] <- NULL
  
  # operations within the list
  for (i in 1:length(dat11)) {
    #i <- 2
    if(type == "classification"){
      
        # classify by 2 classes 'BU', 'BE'
        if(!exists("dfr12")){
          dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% as_tibble(.name_repair = "universal") %>% 
            mutate(LABEL = ifelse(.[[1]]>.[[n]], "BU", "BE"))} else {
            dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% as_tibble(.name_repair = "universal") %>% 
              mutate(LABEL = ifelse(.[[1]]>.[[n]], "BU", "BE")) %>% 
              bind_rows(dfr12)
          }
    } else if(type == "regression"){
      # add label with numeric difference {in pips}
      # i <- 1
      if(!exists("dfr12")){
        dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% as_tibble(.name_repair = "universal", verbose =F) %>%
          mutate(LABEL = 10000*(.[[1]]-.[[n]]))} else {
          dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% as_tibble(.name_repair = "universal", verbose =F) %>%
            mutate(LABEL = 10000*(.[[1]]-.[[n]])) %>% 
            #oldest data will be on top of the dataframe!
            bind_rows(dfr12)
        }
      
      
    }
  }
  
  return(dfr12)
  
}
