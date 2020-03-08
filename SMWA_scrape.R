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


# remove HTML tags
removeHTML <- function(text) {
  clean_texts <- text %>%
    replace_html( ) %>%
    gsub("http.*", "", .)# remove html links replace_html() also works
  return(clean_texts)
}  



# scrape hastags
tweets <- search_tweets("#corona", n = 1000, include_rts = FALSE, token = get_token())
tweets <- tibble(tweets$user_id, tweets$text, tweets$created_at, 
                 tweets$screen_name, tweets$location, timeline = F)
colnames(tweets) <- c('user_id', 'text', 'created_at', 'screen_name', 'location', 'timeline')
tweets$text <- tweets$text %>% removeHTML()
tweets <- tweets %>% distinct()


# scrape timelines
milan <- get_timeline(user = 'MiAirports', n = 3200, token = get_token())
milan <- tibble(milan$user_id, milan$text, milan$created_at, 
                milan$screen_name, milan$location, timeline = T)
colnames(milan) <- c('user_id', 'text', 'created_at', 'screen_name', 'location', 'timeline')
milan$text <- milan$text %>% removeHTML()
milan <- milan %>% distinct()


to_dataset <- bind_rows(tweets, milan)
write_csv(to_dataset, 'dataset.csv', append = T)
