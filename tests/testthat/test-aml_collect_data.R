library(testthat)
library(dplyr)
library(magrittr)
library(lazytrade)
library(lubridate)
library(readr)

context("collect_data")


test_that("collect data works", {

  ind = system.file("extdata", "AI_RSIADXUSDJPY60.csv",
                     package = "lazytrade") %>% read_csv(col_names = F)

  path_data <- normalizePath(tempdir(),winslash = "/")

  aml_collect_data(indicator_dataset = ind,
                    symbol = 'USDJPY',
                    timeframe = 60,
                    path_data = path_data)

  f_name <- "AI_RSIADXUSDJPY60.rds"
  full_path <- file.path(path_data,  f_name)
  AI_RSIADXUSDJPY60 <- read_rds(full_path)

  expect_equal(nrow(AI_RSIADXUSDJPY60), 2200)


})

test_that("data trimming works", {

  path_data <- normalizePath(tempdir(),winslash = "/")
  f_name <- "AI_RSIADXUSDJPY60.rds"
  full_path <- file.path(path_data,  f_name)

  sample(1000) %>% matrix(10,byrow = T) %>% as_tibble(.name_repair = "minimal", verbose =F) %>%
    write_rds(full_path)

  # check number of rows
  x1_nrows <- readr::read_rds(full_path) %>% nrow()
  # what to do if too much rows?
  if(x1_nrows > 8){
    # read all the data
    readr::read_rds(full_path) %>%
      # use only last 9500 rows, 9500 is to avoid this code to run so often...
      utils::head(6) %>%
      # write them back
      readr::write_rds(full_path)
    }

  AI_RSIADXUSDJPY60 <- read_rds(full_path)
  # ---

  expect_equal(nrow(AI_RSIADXUSDJPY60), 6)


})
