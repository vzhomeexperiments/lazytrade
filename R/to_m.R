#' Convert time series data to matrix with defined number of columns
#'
#' @description Transforms Time Series Column of the dataframe to the matrix with specified number of columns.
#' Number of rows will be automatically found. Eventually not complete last row will be discarded
#'
#' @param x - dataframe with one column
#' @param n_cols - number of columns in the matrix
#'
#' @return - matrix with specified amount of rows
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' macd_m <- seq(1:1000) %>% as.data.frame() %>% to_m(64)
#'
#'
to_m <- function(x, n_cols) {
  requireNamespace("tidyverse", quietly = TRUE)

  # get intermediate object and dimension
  Step1 <- x
  # find number of rows of data frame
  nrows <- Step1 %>% nrow()
  # find the number of row in a matrix (Whole Rows), the value will have decimals...
  WN <- nrows/n_cols
  ## extract the whole number uncomment for debug/test
  # WN <- 19.2
  # WN <- 19.8
  if((WN - round(WN)) < 0){WN <- round(WN) - 1} else {WN <- round(WN)}
  # find number of rows to extract data
  n <- n_cols * WN
  # extract relevant matrix
  Step2 <- Step1 %>%
    head(n) %>% #only use whole number to avoid errors
    t() %>%  # this brings us a matrix
    matrix(nrow = WN, ncol = n_cols, byrow = TRUE) # transforming that into matrix size 20x150
  # return the result of the function
  return(Step2)
}

