#' Create initialization files to launch MT4 platform with specific configuration
#'
#' @description Function generate initialization files suitable for launching MT4 terminal with
#' specific parameters. Several options available for generating files specific for each purpose.
#' Option 'prod' will just use existing profile and connect to the broker server
#' Option 'backtest' will generate file for the robot backtest
#' Option 'opt' will generate file needed for the robot optimization
#' Option 'full' allows to specify any desired parameter
#'
#' @details added value of this function is the ability to generate multiple files to backtest several robots
#' for several timeframes. For example it is solves the problem of doing repetitive tasks to 'backtest'
#' robots for several currencies and repeat this procedure over time.
#'
#' Most of the variables present in the function are starting with a prefix mt4_,
#' the remainder of the name comes from the platform documentation, see references
#'
#' Remaining variables are named with a prefix 'dss_' stands for 'Decision Support System', as these
#' are the variables used for further automation purposes
#'
#' Note that for simplicity reasons not all parameters are present in this function.
#' e.g. FTP Settings and Proxy Server settings are not present
#'
#' @references All parameters used are taken from the reference documentation
#' https://www.metatrader4.com/en/trading-platform/help/service/start_conf_file
#'
#' @author (C) 2019 Vladimir Zhbanko
#'
#' @param mt4_Profile string, the subdirectory name in the /profiles directory.
#' The charts will be opened in the client terminal according to the given profile.
#' If this parameter is not specified, the current profile will be opened
#' @param mt4_MarketWatch string, file name (the symbolsets directory) that contains the symbol
#'  list to be shown in the Market Watch window.
#' @param mt4_Login string,  the number of the account to connect to at startup.
#' If this parameter is not specified, the current login will be used.
#' @param mt4_Password string, the password that allows entering the system.
#'  This parameter will be ignored if the client terminal stores personal data on the disk
#'   and the account to be connected is in the list
#' @param mt4_Server string,  the name of the trade server to be connected to.
#' The server name is the same as the name of the corresponding .srv file stored in the /config directory
#' @param mt4_AutoConfiguration string, "true" or "false"
#' depending on whether the autoconfiguration of Data Center setting should be enabled or not.
#' If this parameter is not specified, the value from the current server settings will be used.
#' @param mt4_EnableNews string, either 'false' or 'true'
#' @param mt4_ExpertsEnable string, enable/disable experts.
#' @param mt4_ExpertsDllImport string, enable/disable DLL imports
#' @param mt4_ExpertsExpImport string, enable/disable import of functions from external experts or MQL4 libraries.
#' @param mt4_ExpertsTrades string, enable/disable the experts trading
#' @param mt4_Symbol string,the symbol of the security the chart of which should be opened
#'  immediately after the terminal startup
#' @param mt4_Period string, the chart timeframe (M1, M5, M15, M30, H1, H4, D1, W1, MN).
#' If this parameter is not specified, H1 is used
#' @param mt4_Template string,  the name of the template file (the templates directory),
#' which should be applied to the chart.
#' @param mt4_Expert string, the name of the expert that should be launched after the client terminal has started
#' @param mt4_ExpertParameters string, the name of the file containing the expert parameters
#' (the MQL4 Presets directory).
#' @param mt4_Script string, the name of the script, which must be launched after the client terminal startup
#' @param mt4_ScriptParameters string, the name of the file containing the script parameters
#' (the MQL5 Presets directory).
#' @param mt4_TestExpert string, the name of the expert to be launched for testing.
#'  If this parameter has not been specified, no testing is launched.
#' @param mt4_TestExpertParameters string,  the name of the file containing parameters (the tester directory).
#' @param mt4_TestSymbol string,  the name of the symbol used for the expert testing.
#' If this parameter has not been specified, the latest value used in the tester is used.
#' @param mt4_TestPeriod string, the chart period (M1, M5, M15, M30, H1, H4, D1, W1, MN).
#'  If this parameter has not been specified, H1 is used.
#' @param mt4_TestModel string, 0, 1, or 2, depending on the testing model
#' (Every tick, Control points, Open prices only).
#' If this parameter has not been specified, 0 is used (Every tick)
#' @param mt4_TestSpread string, spread value that will be used for modeling Ask prices during testing.
#' If 0 value is specified, the strategy tester will use the current spread of a symbol at the beginning of testing
#' @param mt4_TestOptimization string, enable/disable optimization. The values that can be taken are "true" or "false".
#'  If this parameter had not been specified, the "false" value is used.
#' @param mt4_TestDateEnable string, enable/disable the "Use date" flag.
#' The values that can be taken are "true" or "false".
#'  If this parameter had not been specified, the "false" value is used.
#' @param mt4_TestFromDate string, the date, from which to start testing, appeared as YYYY.MM.DD.
#'  If this parameter has not been specified, this date is 1970.01.01.
#' @param mt4_TestToDate string, the date, on which to finish testing, appeared as YYYY.MM.DD.
#' If this parameter has not been specified, this date is 1970.01.01.
#' @param mt4_TestReport string, the name of the test report file.
#' The file will be created in the client terminal directory.
#' A relative path can be specified, for example: tester \ MovingAverageReport".
#' If the extension has not been specified in the file name, the ".htm" will be set automatically.
#' If this parameter has not been specified, the test report will not be formed
#' @param mt4_TestReplaceReport string, enable/disable the repeated report file record.
#' The values that can be taken are "true" or "false"
#' @param mt4_TestShutdownTerminal string, enable/disable shutdown of the terminal
#' after the testing has been finished.
#' @param mt4_TestVisualEnable string, enable (true) or disable  (false) the visual test mode.
#'  If the parameter is not specified, the current setting is used.
#' @param dss_inifilepath string, path on the computer where file will be stored
#' @param dss_inifilename string, file name that should be written
#' @param dss_mode string,
#'
#' @return output is a file with desired parameters
#' @export
#'
#' @examples
#'
#' library(lazytrade)
#'
#' dir <- normalizePath(tempdir(),winslash = "/")
#'
#' # test file to launch MT4 terminal with parameters
#' write_ini_file(mt4_Profile = "Default",
#'                mt4_Login = "12345678",
#'                mt4_Password = "password",
#'                mt4_Server = "BrokerServerName",
#'                dss_inifilepath = dir,
#'                dss_inifilename = "prod_T1.ini",
#'                dss_mode = "prod")
#'
#' # test file to launch robot backtest
#' TO <- format(as.Date(Sys.Date()), "%Y.%m.%d")
#' FROM <- format(as.Date(Sys.Date()-60), "%Y.%m.%d")
#'
#' # test file for MT4 use for backtesting
#' write_ini_file(mt4_Profile = "Default",
#'                mt4_Login = "12345678",
#'                mt4_Password = "password",
#'                mt4_Server = "BrokerServerName",
#'                mt4_TestExpert="FALCON_D\\Falcon_D",
#'                mt4_TestExpertParameters="Falcon_D.set",
#'                mt4_TestSymbol="EURUSD",
#'                mt4_TestPeriod="H1",
#'                mt4_TestModel="2",
#'                mt4_TestSpread="20",
#'                mt4_TestOptimization="false",
#'                mt4_TestDateEnable="true",
#'                mt4_TestFromDate=FROM,
#'                mt4_TestToDate=TO,
#'                mt4_TestReport="EURUSD_Report",
#'                mt4_TestReplaceReport="false",
#'                mt4_TestShutdownTerminal="true",
#'                mt4_TestVisualEnable="false",
#'                dss_inifilepath = dir,
#'                dss_inifilename = "backtest.ini",
#'                dss_mode = "backtest")
#'
#'
write_ini_file <- function(mt4_Profile="Default",
                           mt4_MarketWatch="Forex.set",
                           mt4_Login="1234567",
                           mt4_Password="xxxxxXX",
                           mt4_Server="BrokerServerName",
                           mt4_AutoConfiguration="false",
                           mt4_EnableNews="false",
                           mt4_ExpertsEnable="true",
                           mt4_ExpertsDllImport="true",
                           mt4_ExpertsExpImport="true",
                           mt4_ExpertsTrades="true",
                           mt4_Symbol="EURUSD",
                           mt4_Period="H1",
                           mt4_Template="Default",
                           mt4_Expert="",
                           mt4_ExpertParameters="",
                           mt4_Script="",
                           mt4_ScriptParameters="",
                           mt4_TestExpert="",
                           mt4_TestExpertParameters="",
                           mt4_TestSymbol="EURUSD",
                           mt4_TestPeriod="H1",
                           mt4_TestModel="",
                           mt4_TestSpread="",
                           mt4_TestOptimization="false",
                           mt4_TestDateEnable="true",
                           mt4_TestFromDate="",
                           mt4_TestToDate="",
                           mt4_TestReport="test report",
                           mt4_TestReplaceReport="false",
                           mt4_TestShutdownTerminal="",
                           mt4_TestVisualEnable="false",
                           dss_inifilepath="",
                           dss_inifilename="test.ini",
                           dss_mode="prod"){



# # full path to the file
out_file_path <- file.path(dss_inifilepath, dss_inifilename)

## ==== generate file for 'prod' ====
content1 <- c("; common settings",
              paste0("Profile=",mt4_Profile),
              paste0("MarketWatch=",mt4_MarketWatch),
              paste0("Login=",mt4_Login),
              paste0("Password=",mt4_Password),
              paste0("Server=",mt4_Server),
              paste0("AutoConfiguration=",mt4_AutoConfiguration),
              paste0("EnableNews=",mt4_EnableNews))

## ==== generate file for 'backtest' ====
content2 <- c("; common settings",
              paste0("Profile=",mt4_Profile),
              paste0("MarketWatch=",mt4_MarketWatch),
              paste0("Login=",mt4_Login),
              paste0("Password=",mt4_Password),
              paste0("Server=",mt4_Server),
              paste0("AutoConfiguration=",mt4_AutoConfiguration),
              paste0("EnableNews=",mt4_EnableNews),
              "; start strategy tester",
              paste0("TestExpert=",mt4_TestExpert),
              paste0("TestExpertParameters=",mt4_TestExpertParameters),
              paste0("TestSymbol=",mt4_TestSymbol),
              paste0("TestPeriod=",mt4_TestPeriod),
              paste0("TestModel=",mt4_TestModel),
              paste0("TestSpread=",mt4_TestSpread),
              paste0("TestOptimization=false"),
              paste0("TestDateEnable=",mt4_TestDateEnable),
              paste0("TestFromDate=",mt4_TestFromDate),
              paste0("TestToDate=",mt4_TestToDate),
              paste0("TestReport=",mt4_TestReport),
              paste0("TestReplaceReport=",mt4_TestReplaceReport),
              paste0("TestShutdownTerminal=",mt4_TestShutdownTerminal),
              paste0("TestVisualEnable=",mt4_TestVisualEnable))

## ==== generate file for 'opt' ====
content3 <- c("; common settings",
              paste0("Profile=",mt4_Profile),
              paste0("MarketWatch=",mt4_MarketWatch),
              paste0("Login=",mt4_Login),
              paste0("Password=",mt4_Password),
              paste0("Server=",mt4_Server),
              paste0("AutoConfiguration=",mt4_AutoConfiguration),
              paste0("EnableNews=",mt4_EnableNews),
              "; start strategy tester",
              paste0("TestExpert=",mt4_TestExpert),
              paste0("TestExpertParameters=",mt4_TestExpertParameters),
              paste0("TestSymbol=",mt4_TestSymbol),
              paste0("TestPeriod=",mt4_TestPeriod),
              paste0("TestModel=",mt4_TestModel),
              paste0("TestSpread=",mt4_TestSpread),
              paste0("TestOptimization=true"),
              paste0("TestDateEnable=",mt4_TestDateEnable),
              paste0("TestFromDate=",mt4_TestFromDate),
              paste0("TestToDate=",mt4_TestToDate),
              paste0("TestReport=",mt4_TestReport),
              paste0("TestReplaceReport=",mt4_TestReplaceReport),
              paste0("TestShutdownTerminal=",mt4_TestShutdownTerminal),
              paste0("TestVisualEnable=",mt4_TestVisualEnable))

## ==== generate file for 'full' ====
content4 <- c("; common settings",
              paste0("Profile=",mt4_Profile),
              paste0("MarketWatch=",mt4_MarketWatch),
              paste0("Login=",mt4_Login),
              paste0("Password=",mt4_Password),
              paste0("Server=",mt4_Server),
              paste0("AutoConfiguration=",mt4_AutoConfiguration),
              paste0("EnableNews=",mt4_EnableNews),
              "; experts settings",
              paste0("ExpertsEnable=",mt4_ExpertsEnable),
              paste0("ExpertsDllImport=",mt4_ExpertsDllImport),
              paste0("ExpertsExpImport=",mt4_ExpertsExpImport),
              paste0("ExpertsTrades=",mt4_ExpertsTrades),
              "; open chart and run expert and/or script",
              paste0("Symbol=",mt4_Symbol),
              paste0("Period=",mt4_Period),
              paste0("Template=",mt4_Template),
              paste0("Expert=",mt4_Expert),
              paste0("ExpertParameters=",mt4_ExpertParameters),
              paste0("Script=",mt4_Script),
              paste0("ScriptParameters=",mt4_ScriptParameters),
              "; start strategy tester",
              paste0("TestExpert=",mt4_TestExpert),
              paste0("TestExpertParameters=",mt4_TestExpertParameters),
              paste0("TestSymbol=",mt4_TestSymbol),
              paste0("TestPeriod=",mt4_TestPeriod),
              paste0("TestModel=",mt4_TestModel),
              paste0("TestSpread=",mt4_TestSpread),
              paste0("TestOptimization=",mt4_TestOptimization),
              paste0("TestDateEnable=",mt4_TestDateEnable),
              paste0("TestFromDate=",mt4_TestFromDate),
              paste0("TestToDate=",mt4_TestToDate),
              paste0("TestReport=",mt4_TestReport),
              paste0("TestReplaceReport=",mt4_TestReplaceReport),
              paste0("TestShutdownTerminal=",mt4_TestShutdownTerminal),
              paste0("TestVisualEnable=",mt4_TestVisualEnable))

## ==== end of generate file ====
if(dss_mode == 'prod'){
  writeLines(content1, out_file_path)}
if(dss_mode == 'backtest'){
  writeLines(content2, out_file_path)}
if(dss_mode == 'opt'){
  writeLines(content3, out_file_path)}
if(dss_mode == 'full'){
  writeLines(content4, out_file_path)}

}
