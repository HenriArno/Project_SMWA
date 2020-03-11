# Load Packages -----------------------------------------------------------

# Clean Workspace and setwd
rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(httr,rtweet,tidyverse,textclean, textstem, sentimentr, lexicon, maps, dplyr)


# Scrape Tweets - Write to CSV --------------------------------------------

# Initialize Scraper 
source('tokensandkeys.R')
get_token() #test if it works


# remove HTML tags
removeHTML <- function(text) {
  clean_texts <- text %>%
    replace_html( ) %>%
    gsub("http.*", "", .)# remove html links replace_html() also works
  return(clean_texts)
}  



# scrape hashtags
scrape_hashtags <- function (hashtag, numberOfTweets) {
  tweets <- search_tweets(hashtag, n = numberOfTweets, include_rts = FALSE, token = get_token())
  tweets <- tibble(tweets$user_id, tweets$text, tweets$created_at, 
                   tweets$screen_name, tweets$location, timeline = F)
  colnames(tweets) <- c('user_id', 'text', 'created_at', 'screen_name', 'location', 'timeline')
  tweets$text <- tweets$text %>% removeHTML()
  tweets <- tweets %>% distinct()
  write_csv(tweets, 'dataset.csv', append = T)
}


# scrape timelines
scrape_timelines <- function (timeline, numberOfTweets) {
  tweets <- get_timeline(user = timeline, n = numberOfTweets, token = get_token())
  tweets <- tibble(tweets$user_id, tweets$text, tweets$created_at, 
                   tweets$screen_name, tweets$location, timeline = T)
  colnames(tweets) <- c('user_id', 'text', 'created_at', 'screen_name', 'location', 'timeline')
  tweets$text <- tweets$text %>% removeHTML()
  tweets <- tweets %>% distinct()
  write_csv(tweets, 'dataset.csv', append = T)
}

# Actual Scrapping  --------------------------------------------

hashtags = as.vector(t(read.delim("hashtags.txt", header = F)))
timelines = as.vector(t(read.delim("timelines.txt", header = F)))

for (element in hashtags) {
  scrape_hashtags(element, 1600)
}

for (element in timelines) {
  scrape_timelines(element, 1600)
}
  
