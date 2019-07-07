#' Create labelled data
#'
#' @description FUNCTION create_labelled_data.
#' PURPOSE: function gets price data of every currency in each column.
#' It is splitting this data by periods and transposes the data.
#' Additionally function is capable to label the data based on the simple logic.
#' Each row will be assigned into 2 categories based on the difference between beginning and end of the row elements
#' Finally all data will be stacked on top and joined into the table
#'
#' Learn by example how to manipulate data
#'
#' @details see more info in the udemy course self-learning-trading-robot
#'
#' @param x - data set containing a table where 1st column is a Time index and other columns containing financial asset price values
#' @param n - number of rows we intend to split and transpose the data to
#' @param type - type of the label required. Can be either "classification" or "regression".
#' "classification" will return either "BU" or "BE",
#' "regression" will return the difference between first value and the last value in each row (in pips)
#'
#' @return function returns transposed data. One column called 'LABEL' indicate achieved value of the label.
#' Transposed values from every column are stacked one to each other
#'
#' @export
#'
#' @examples
#'
#'
#' library(tidyverse)
#'
#' # usind a sample data
#' data(price_dataset)
#'
#' # price change as a label
#' create_labelled_data(price_dataset, n = 75, type = "regression")
#'
#' # factors 'BU'/'BE' as a label
#' create_labelled_data(price_dataset, n = 75, type = "classification")
#'
#'
create_labelled_data <- function(x, n = 50, type = "regression"){
  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  # x <- price_dataset
  # n <- 50
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
