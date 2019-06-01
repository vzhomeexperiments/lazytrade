# register for developer account via https://apps.twitter.com
# obtain keys by creating an App in from https://developer.twitter.com/en/apps
# encrypt your keys using script 'encrypt_api_key.R'

#' Function to return sentiment graph or resulting table
#'
#' @param search_term 
#' @param n_tweets 
#'
#' @return
#' @export
#'
#' @examples
get_twitter_sentiment <- function(search_term = 'tesla',
                                  n_tweets = 1500,
                                  output_df = F){

require(twitteR)
require(tidyverse)
require(syuzhet)
require(lubridate)
require(scales)
require(reshape2)

  # test zone
  # search_term <- "tesla"
  # n_tweets <- 1500
  # output_df <- TRUE
  # output_df <- FALSE
  

## =================================================================

# --------- get a list of tweets with searched term
tweets_df <- searchTwitter(searchString = search_term,
                           n = n_tweets,
                           lang = "en",
                           since = NULL,
                           until = NULL) %>%
  twListToDF()

## ==================================================================

## Sentiment analysis

#obtain sentiment scores
sent_scores <- get_nrc_sentiment(tweets_df$text)

#bar plot
  barplot(colSums(sent_scores),
          las = 2, 
          col = rainbow(10),
          ylab = 'Count',
          main = paste0('Tweet Sentiment Scores ', search_term))

# return sentiment scores as a dataframe
if(output_df == TRUE) return(sent_scores) 
  


}