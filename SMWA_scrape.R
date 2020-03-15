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
  tweets <- search_tweets(hashtag, n = numberOfTweets, include_rts = FALSE, token = get_token(), lang = 'en')
  tweets <- tibble(tweets$user_id, tweets$text, tweets$created_at, 
                   tweets$screen_name, tweets$location, timeline = F)
  colnames(tweets) <- c('user_id', 'text', 'created_at', 'screen_name', 'location', 'timeline')
  tweets$text <- tweets$text %>% removeHTML()
  tweets <- tweets %>% distinct()
  write_csv(tweets, 'dataset.csv', append = T)
}

# Actual Scrapping  --------------------------------------------

hashtags = as.vector(t(read.delim("hashtags.txt", header = F)))

#make a subset of hashtags here to scrape
#Pieter     [1:2]
#Henri      [3:4]
#Céline     [5:6]
#Thibault   [7:8]
#Adriaan    [9:10]
#Seppe      [11:12]


hashtags = hashtags[1:2]

for (element in hashtags) {
  scrape_hashtags(element, 3200/length(hashtags))
}

