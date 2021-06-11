.onLoad <- function(libname = find.package("lazytrade"), pkgname = "lazytrade") {

  #op <- options()
  #the_tempdir <- tempdir()

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # sample variable names from custom functions
      c("path_trading_robot", "num_trades_to_consider", "profit_factor_limit",
        "MagicNumber", "OrderCloseTime", "row_number", "PrFact",
        "AchievedGain", "AchievedPnL", "ExpectedGain", "ExpectedPnL",
        "FinalQuality", "Gain", "IsEnabled", "LABEL", "Loss", "Magic",
        "MagicNumber.x", "NetGain", "OFF", "ON", "Policy",
        "Prev_result", "Profit", "Risk", "RiskEstim", "RiskKnown",
        "Spread", "T3_PnL", "T3_trigger", "TPSL_Level", "Tot_pnlT3", "TotalTrades", "TradeState",
        "X1", "X10", "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19",
        "X2", "X20", "X21", "X22", "X23", "X24", "X25", "X26", "X27", "X28", "X29",
        "X3", "X4", "X5", "X6", "X7", "X8", "X9", "alpha", "any_vars",
        "computePolicy", "decrypt_envelope", "epsilon",
        "funs", "n", "nOrders",  "path_T1", "path_T3","path_model", "path_data",
        "rewardseq.OFF", "rewardseq.ON",
        "totreward", "trstate", "MarketType","predict_CMSUM", "LABEL_CMSUM",
        "CUMSUM_PNL", "DFR", "PairGain", "Symbol", "aes", "asset_name",

        "Hold_NB", "MaxPerf", "NB_hold", "PnL_NB", "TR_Level", "X2_NB",
        "qrtl", "value","FrstQntlPerf", "col_number", "t_running",


        # sample function names
        ".", "%>%", "%$%", "group_by",  "arrange", "desc",
        "filter", "ungroup", "select", "mutate", "inner_join",
        "read_csv", "write_csv", "as_tibble", "ReinforcementLearning", "na.omit",
        "h2o.deeplearning", "h2o.loadModel", "h2o.predict", "h2o.saveModel", "head", "tail",
        "if_else", "lag", "mutate_all", "mutate_if", "slice", "str_detect",
        "predict", "read_rds", "summarise", "write.csv", "write_rds", "ymd_hms",
        "filter_all", "full_join", "bind_rows",  "as.h2o", "bind_cols", "testdir",
        "h2o.performance", "distinct", "dev.off", "geom_bar", "geom_line", "ggplot", "ggtitle", "pdf",
        "sha512", "write_tsv", "encrypt_envelope", "mutate_at", "across", "rename"
      )
    )




  invisible(NULL)
}



# .onUnload <- function (libpath) {
#   library.dynam.unload("lazytrade", libpath)
# }
