
# Random Forest Approach to Sentiment -------------------------------------

# In this script we will firstly make a dataset based on the positive and negative emojis present in our raw
# data. Next we will train a Random Forest model to identify sentiment and then apply said model to the rest
# of our data.


# Setting wd and clearing environmnet -------------------------------------

rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# loading required packages -----------------------------------------------

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse,Unicode,tm, rvest, rtweet, stringr)

# Reading in data ---------------------------------------------

#reading in dataset as tibble
data <- read.csv("dataset.csv", stringsAsFactors = F)
colnames(data) <- c('user_id', 'text', 'timestamp', 'screenname', 'location', 'timeline')
data$timeline <- NULL
data <- as_tibble(data)

# adjust part of datadrame from row 9.362 untill 23.213 (due to bug in scrape function) 
indices <- c(9362:23213)
data[indices,'text'] <- data[indices, 'user_id']
data[indices, c('user_id', 'screenname', 'location')] <- NA
data[indices, c('timestamp')] <- '2020-03-13'
rm(indices)
# Extract text from tibble
data <- data %>% slice(1:2000)
text <- data %>% select(text)

#transforming data to data we can work with
text <- text %>% mutate(text = iconv(text, from = "latin1", to = "ascii", sub = "byte"))


# constructing emoji dictionary -------------------------------------------

#I used the code linked to this article: https://www.r-bloggers.com/emojis-analysis-in-r/
#It constructs a dictionary for emojis 


# read in emoji dictionary
# I used to get the dictionary from Felipe: https://github.com/felipesua
# but he put it down, so I uploaded the csv file to my github profile: 
# https://raw.githubusercontent.com/today-is-a-good-day/emojis/master/emojis.csv
# input your custom path to file
emDict_raw <- read.csv2("./sources/raw/emojis.csv") %>% 
  select(description = EN, r_encoding = ftu8, unicode)

# plain skin tones
skin_tones <- c("light skin tone", 
                "medium-light skin tone", 
                "medium skin tone",
                "medium-dark skin tone", 
                "dark skin tone")

# remove plain skin tones and remove skin tone info in description
emDict <- emDict_raw %>%
  # remove plain skin tones emojis
  filter(!description %in% skin_tones) %>%
  # remove emojis with skin tones info, e.g. remove woman: light skin tone and only
  # keep woman
  filter(!grepl(":", description)) %>%
  mutate(description = tolower(description)) %>%
  mutate(unicode = gsub(' [A-z ]*', '' , unicode)) %>%
  mutate(unicode = as.u_char(unicode))
# all emojis with more than one unicode codepoint become NA 

matchto <- as.character(emDict$r_encoding)
description <- emDict$description

#the next part scrapes the results of a paper which adds valence scores to emojis
# reference website
url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"

# get emoticons
emojis_raw <- url %>%
  read_html() %>%
  html_table() %>%
  data.frame() %>%
  select(-Image.twemoji., -Sentiment.bar.c.i..95..)
names(emojis_raw) <- c("char", "unicode", "occurrences", "position", "negative", 
                       "neutral", "positive", "sentiment_score", "description", 
                       "block")
# change numeric unicode to character unicode to be able to match with emDict 
emojis <- emojis_raw %>%
  mutate(unicode = as.u_char(unicode)) %>%
  mutate(description = tolower(description)) 


# merge with emDict to get encoding
emojis_merged <- emojis %>%
  merge(emDict, by = "unicode")
# emojis %>% filter(!unicode %in% emDict$unicode) %>% View
# we lose 137 emojis that are not in emDict and for which we don't have an R encoding
# but they seem to be black and white emojis not too often used in social media anyways

#construct dictionary based on sourced information from code above
dict_full <- emojis_merged %>% select(sentiment_score, unicode, description.x, r_encoding, char)
dict <- dict_full %>% select(sentiment_score, r_encoding)
#delete remainders unused variables
rm(emDict, emDict_raw, emojis, emojis_merged, emojis_raw, description, matchto, skin_tones, url)



# Construct labeled dataset based on emoji --------------------------------------------------

emoji_regex <- paste0(dict$r_encoding, collapse="|")

score <- numeric((dim(text))[1])
scores <- tibble() %>% add_column(text =NA) %>% add_column(score=NA)

for (i in 1:length(score)){
  
  #select entry
  entry <- (text%>% slice(i))[[1]]
  
  #select the emojis in the entry
  emojis <- str_extract_all(entry, emoji_regex)
  
  #find the positions of the words in the Tweet in the dictionary
  m <- match(emojis, dict$r_encoding)
  
  #which words are present in the dictionary?
  present <- !is.na(m)
  #tweetsplit[present]
  
  #of the words that are present, select their valence
  wordvalences <- dict$sentiment_score[m[present]]
  
  #compute the mean valence of the tweet
  score[i] <- mean(wordvalences, na.rm=TRUE)
  
  #handle the case when none of the words is in the dictionary
  if (is.na(score[i])) score[i] <- 0 
  else {
    score[i] <- score[i]
    scores <- scores %>% add_row(text = entry, score = score[i])
    }
  
}

write.csv(scores, "./sources/raw/training set.csv")
