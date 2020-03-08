# Load Packages -----------------------------------------------------------

# Clean Workspace and setwd
rm(list=ls())
setwd("~/Desktop/Project_SMWA")

# Load Packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(httr,rtweet,tidyverse,textclean, textstem, sentimentr, lexicon, maps, dplyr)


# Scrape Tweets - Write to CSV --------------------------------------------

# Initialize Scraper 
source('tokensandkeys.R')
get_token() #test if it works

# Scrape and write to CSV

# Get the tweets
tweets <- search_tweets("#corona", n = 1000, include_rts = FALSE, 
              token = get_token())
tweets <- tibble(tweets$user_id, tweets$text)
colnames(tweets) <- c('user_id', 'text')

# Remove duplicates (but first remove html links)
removeHTML <- function(text) {
  clean_texts <- text %>%
    replace_html( ) %>%
    gsub("http.*", "", .)# remove html links replace_html() also works
    return(clean_texts)
}  
tweets$text <- tweets$text %>% removeHTML()
tweets <- tweets %>% distinct()


# Write to CSV
write_csv(tweets, 'dataset.csv', append = T)



