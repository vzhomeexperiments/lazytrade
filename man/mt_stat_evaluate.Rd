% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mt_stat_evaluate.R
\name{mt_stat_evaluate}
\alias{mt_stat_evaluate}
\title{Function to prepare and score data, finally predict current market type using pre-trained classification model}
\usage{
mt_stat_evaluate(x, path_model, num_bars, timeframe)
}
\arguments{
\item{x}{\itemize{
\item dataframe with one column containing asset indicator in the time descending order,
typically 64 or more values
}}

\item{path_model}{String, path to the model}

\item{num_bars}{Integer, Number of bars used to perform transformation}

\item{timeframe}{Integer, timeframe in Minutes.}
}
\value{
dataframe with predicted value of the market type
}
\description{
PURPOSE: Function that uses Deep Learning model and Time Series Column of the dataframe
to find out specific market type of the financial asset
it will also discard bad result outputting -1 if it is the case

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\examples{

\donttest{

library(h2o)
library(magrittr)
library(dplyr)
library(readr)
library(lazytrade)
library(stats)

path_model <- normalizePath(tempdir(),winslash = "/")
path_data <- normalizePath(tempdir(),winslash = "/")


# start h2o engine (using all CPU's by default)
h2o.init(nthreads = 2)

data(price_dataset_big)
data <- head(price_dataset_big, 500) #reduce computational time

ai_class <- mt_stat_transf(indicator_dataset = data,
                      num_bars = 64,
                      timeframe = 60,
                      path_data = path_data,
                      mt_classes = c('BUN', 'BEN', 'RAN'))

# performing Deep Learning Classification using the custom function auto clustered data
mt_make_model(indicator_dataset = ai_class,
              num_bars = 64,
              timeframe = 60,
              path_model = path_model,
              path_data = path_data,
              activate_balance = TRUE,
              num_nn_options = 3,
              num_epoch = 10,
              is_cluster = TRUE)


# Use sample data
data(price_dataset)

# use one column for testing
x <- price_dataset[ ,2]


mt_stat_evaluate(x = x,
                 path_model = path_model,
                 num_bars = 64,
                 timeframe = 60)

h2o.shutdown(prompt = FALSE)

#set delay to insure h2o unit closes properly before the next test
Sys.sleep(5)

}

}
\author{
(C) 2021 Vladimir Zhbanko
}
