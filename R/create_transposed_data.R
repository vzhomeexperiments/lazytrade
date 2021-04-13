#' Create Transposed Data
#'
#' @description PURPOSE: function gets indicator data in each column.
#' Goal is to splitting this data into periods and transpose the data.
#'
#' `r lifecycle::badge('superseded')`
#'
#' @details each column contains records of the indicator value of the assets
#' every column will be split into chunks of n observations and transposed into rows
#' this repeated for all the columns coming up with a matrix.
#' Function works in combination with a function create_labelled_data
#'
#'
#' @param x - data set containing a table where 1st column is a Time index and other columns containing financial asset indicator values
#' @param n - number of rows we intend to split and transpose the data
#'
#' @return function returns transposed data. Transposed values from every column are stacked one to each other
#'
#' @export
#'
#' @examples
#'
#'
#' library(dplyr)
#' library(magrittr)
#' library(lazytrade)
#'
#' # usind a sample data
#' data(indicator_dataset)
#'
#' create_transposed_data(indicator_dataset, n = 75)
#'
#'
#'
create_transposed_data <- function(x, n = 50){

  requireNamespace("dplyr", quietly = TRUE)
  #
  #n <- 100
  #x <- indicator_dataset

  nr <- nrow(x)
  namesdfr12 <- paste0("X", 1:n) #generated names for dataset useful later in the code

  dat11 <- x %>% dplyr::select(-1) %>% split(rep(1:ceiling(nr/n), each=n, length.out=nr)) #list
  dat11[length(dat11)] <- NULL

  # operations within the list
  for (i in 1:length(dat11)) {
    #i <- 1

    if(!exists("dfr12")){
      dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% dplyr::as_tibble(.name_repair = "minimal", verbose =F)
      names(dfr12) <- namesdfr12
      } else {
        dfr12 <- dat11[i] %>% as.data.frame() %>% t() %>% dplyr::as_tibble(.name_repair = "minimal", verbose =F)
          names(dfr12) <- namesdfr12
          dfr12 <- dfr12 %>% dplyr::bind_rows(dfr12)
      }

  }

  return(dfr12)

}

