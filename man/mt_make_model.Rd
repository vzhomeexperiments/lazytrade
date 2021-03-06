% Generated by roxygen2: do not edit by hand
% Please edit documentation in www.vantharp.com/>
\name{mt_make_model}
\alias{mt_make_model}
\title{Function to train Deep Learning Classification model for Market Type recognition}
\usage{
mt_make_model(
  indicator_dataset,
  num_bars,
  timeframe = 60,
  path_model,
  path_data,
  activate_balance = TRUE,
  num_nn_options = 24,
  is_cluster = FALSE
)
}
\arguments{
\item{indicator_dataset}{Dataframe, Dataset containing indicator patterns to train the model}

\item{num_bars}{Integer, Number of bars used to detect pattern}

\item{timeframe}{Integer, Data timeframe in Minutes.}

\item{path_model}{String, Path where the models are be stored}

\item{path_data}{String, Path where the aggregated historical data is stored, if exists in rds format}

\item{activate_balance}{Boolean, option to choose if to balance market type classes or not, default TRUE}

\item{num_nn_options}{Integer, value from 3 to 24 or more. Used to change number of variants
of the random neural network structures. Value 3 will mean that only one
random structure will be used. To avoid warnings make sure to set this value
multiple of 3. Higher values will increase computation time.}

\item{is_cluster}{Boolean, set TRUE to use automatically clustered data}
}
\value{
Function is writing file object with the model
}
\description{
Function is training h2o deep learning model to match manually classified patterns of the financial
indicator. Main idea is to be able to detect Market Type by solely relying on the current indicator pattern.
This is in the attempt to evaluate current market type and to use proper trading strategy.
Function will always try to gather mode data to update the model.

Selected Market Periods according to the theory from Van K. Tharp:
1. Bull normal, BUN
2. Bull volatile, BUV
3. Bear normal, BEN
4. Bear volatile, BEV
5. Sideways quiet, RAN
6. Sideways volatile, RAV
}
\details{
Function is using manually prepared dataset and tries several different random neural network structures.
Once the best neural network is found then the better model is trained and stored.
}
\examples{

\donttest{

library(dplyr)
library(magrittr)
library(readr)
library(h2o)
library(lazytrade)
library(stats)

path_model <- normalizePath(tempdir(),winslash = "/")
path_data <- normalizePath(tempdir(),winslash = "/")

data(macd_ML60M)

Sys.sleep(5)

# start h2o engine
h2o.init(nthreads = 2)


# performing Deep Learning Classification using the custom function manually prepared data
mt_make_model(indicator_dataset = macd_ML60M,
              num_bars = 64,
              timeframe = 60,
              path_model = path_model,
              path_data = path_data,
              activate_balance = TRUE,
              num_nn_options = 3)

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
              is_cluster = TRUE)


# stop h2o engine
h2o.shutdown(prompt = FALSE)

#set delay to insure h2o unit closes properly before the next test
Sys.sleep(5)

}



}
\author{
(C) 2020 Vladimir Zhbanko
}
