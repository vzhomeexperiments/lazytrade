#' Function to create summary graphs of the trading results
#'
#' @description Create graphs and store them into pdf file
#'
#' `r lifecycle::badge('stable')`
#'
#' @details bar graph and time series optionally written to the pdf file.
#' File is named with a date of analysis to the location specified by the user
#'
#' @param x - dataframe with aggregated trading results
#' @param outp_path - path to the folder where to write file
#' @param graph_type - character, one of the options c('ts', 'bars', 'pdf')
#'
#' @return graphic output
#'
#' @export
#'
#' @examples
#'
#'  library(lazytrade)
#'  library(readr)
#'  library(dplyr)
#'  library(magrittr)
#'  library(lubridate)
#'  library(ggplot2)
#'  data(DFR)
#'  dir <- normalizePath(tempdir(),winslash = "/")
#'  # create pdf file with two graphs
#'  opt_create_graphs(x = DFR, outp_path = dir)
#'
#'  # only show time series plot
#'  opt_create_graphs(x = DFR, graph_type = 'ts')
#'
#'
opt_create_graphs <- function(x, outp_path, graph_type = "pdf"){

  requireNamespace("ggplot2", quietly = TRUE)

  # generate bar plot
  bars <- x %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::group_by(Symbol) %>%
    dplyr::summarise(PairGain = sum(Profit)) %>%
    ggplot2::ggplot(aes(x = Symbol, y = PairGain))+ggplot2::geom_bar(stat = "identity")

  # generate time series plot
  # extract currency pairs used
  pairs_used <- unique(DFR$Symbol) %>% paste(collapse = " ")
  ts <- x %>% ggplot2::ggplot(ggplot2::aes(x = OrderCloseTime, y = CUMSUM_PNL))+
    ggplot2::geom_line()+ ggplot2::ggtitle(paste("Using pairs: ", pairs_used))

  if(graph_type == "ts"){  print(ts)  }

  if(graph_type == "bars"){ print(bars) }


  if(graph_type == "pdf"){

    grDevices::pdf(file = file.path(outp_path, paste0(Sys.Date(), ".pdf")))
    print(ts)
    print(bars)
    grDevices::dev.off()
  }



}
