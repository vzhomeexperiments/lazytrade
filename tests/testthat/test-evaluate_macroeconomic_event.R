
library(testthat)
library(dplyr)
library(readr)
library(lazytrade)





dir <- path_T1 <- path_T3 <- normalizePath(tempdir(),winslash = "/")
setup_file_path = system.file('extdata', package = "lazytrade")
setup_file_name = "Setup.csv"
macro_event_path = system.file('extdata', package = "lazytrade")
macro_file_name = "01_MacroeconomicEvent.csv"

# define path to the setup file
setup_complete_path <- file.path(setup_file_path, setup_file_name)

# define path to the macro event file
macro_complete_path <- file.path(macro_event_path, macro_file_name)

  #read the file containing a flag (1 will mean that event is present hence no new opened orders)
DF_NT <- readr::read_csv(file= macro_complete_path, col_types = "i")
#read the table of trading robots in operation
DF_Setup <- readr::read_csv(setup_complete_path, col_types = "ffi")

context("import_data")

test_that("file read works", {

  #read the file containing a flag (1 will mean that event is present hence no new opened orders)
  DF_NT <- readr::read_csv(file= macro_complete_path, col_types = "i")
  #read the table of trading robots in operation
  DF_Setup <- readr::read_csv(setup_complete_path, col_types = "ffi")


expect_s3_class(DF_NT, "tbl")
expect_s3_class(DF_Setup, "tbl")

})

context("data manipulation")


test_that("manipulation works", {


  #read the file containing a flag (1 will mean that event is present hence no new opened orders)
  DF_NT <- readr::read_csv(file= macro_complete_path, col_types = "i")
  #read the table of trading robots in operation
  DF_Setup <- readr::read_csv(setup_complete_path, col_types = "ffi")

   # disable trades in T3
  res <- DF_Setup %>% dplyr::group_by(Magic) %>%
     dplyr::mutate(MagicNumber = Magic + 200, IsEnabled = 0) %>%
     dplyr::group_by(MagicNumber) %>%
     dplyr::select(MagicNumber, IsEnabled)

res$IsEnabled %>% sum()

    expect_equal(sum(res$IsEnabled), 0)

})
