library(testthat)


context("write_data")


test_that("content creation works", {


  mt4_Profile <- "Default"
  mt4_MarketWatch <- "Forex.set"
  mt4_Login <- "1234567"
  mt4_Password <- "xxxxxXX"
  mt4_Server <- "BrokerServerName"
  mt4_AutoConfiguration <- "false"
  mt4_EnableNews <- "false"

  ## ==== generate file for 'prod' ====
  content1 <- c("; common settings",
                paste0("Profile=",mt4_Profile),
                paste0("MarketWatch=",mt4_MarketWatch),
                paste0("Login=",mt4_Login),
                paste0("Password=",mt4_Password),
                paste0("Server=",mt4_Server),
                paste0("AutoConfiguration=",mt4_AutoConfiguration),
                paste0("EnableNews=",mt4_EnableNews))

  expect_length(content1, n = 8)
  expect_type(content1, "character")

})
