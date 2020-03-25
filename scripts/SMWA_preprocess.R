# setwd and load packages -------------------------------------------------
rm(list=ls())
library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
# Load Packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(httr,rtweet,tidyverse,textclean, textstem, sentimentr, lexicon, maps, dplyr)


# create tibble -----------------------------------------------------------
data <- read.csv("./sources/raw/dataset.csv", stringsAsFactors = F)
colnames(data) <- c('user_id', 'text', 'timestamp', 'screenname', 'location', 'timeline')
data$timeline <- NULL
data <- as_tibble(data)


# Debug Mode --------------------------------------------------------------

# Place following line in comment if you want to perform data manipulations on entire dateset 
# otherwise we only look at the first 200 entries 
#data <- data %>% slice(., 1:200)

# impute missing data ---------------------------------------------------------

# adjust part of datadrame from row 9.362 untill 23.213 (due to bug in scrape function) 
indices <- c(9362:23213)
data[indices,'text'] <- data[indices, 'user_id']
data[indices, c('user_id', 'screenname', 'location')] <- NA
data[indices, c('timestamp')] <- '2020-03-13'

# Preprocess data ---------------------------------------------------------

# Extract text from tibble
text <- data$text

# Preprocess steps

# 1. convert encoding 
text <- iconv(text, from = "latin1", to = "ascii", sub = "byte")

# 2. function to clean text 
cleanText <- function(text) {
  clean_texts <- text %>%
    gsub("&amp;", "", .) %>% # remove &
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("@\\w+", "", .) %>% # remove at people replace_at() also works
    #gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    gsub('#', "", .) %>% #remove only hashtag 
    #gsub("<.*>", "", .) %>% # remove remainig emojis
    #gsub("(?:\\s*#\\w+)+\\s*$", "", .) %>% #remove hashtags in total
    #gsub("http\\w+", "", .) %>% # remove html links replace_html() also works
    tolower
  return(clean_texts)
}

remove_topics <- function(text) {
  clean_texts <- text %>%
    gsub('covid', '', .) %>%
    gsub('corona', '', .) %>%
    gsub('covid19', '', .) %>%
    gsub('corona19', '', .) %>%
    gsub('COVID', '', .) %>%
    gsub('CORONA', '', .) %>%
    gsub('COVID19', '', .) %>%
    gsub('virus', '', .) %>%
    gsub('CORONA19', '',.) %>%
    gsub('pandemia', '',.) %>%
    gsub('pandemic', '', .)
  return(clean_texts)
}

# applt the cleaning functions and save the text in text_clean
text_clean <- cleanText(text) %>% replace_contraction() %>%
  replace_internet_slang() %>% replace_kern() %>% replace_word_elongation()

# 3. Perform lemmatization
lemma_dictionary_hs <- make_lemma_dictionary(text_clean,
                                             engine = 'hunspell')
text_clean <- lemmatize_strings(text_clean, dictionary = lemma_dictionary_hs)


# 4. Also remove the topics for topicmodeling
topic_text <- remove_topics(text_clean)





# 5. store the cleaned text in the dataset and write to csv
data$text <- text_clean
data <- data[!duplicated(data$text),]

#Save clean data
write_csv(data, './sources/cleaned/dataset_cleaned.csv')



# 6. store the cleaned and topic-removed text in the dataset and write to csv
# note that we have altered 'data' in the previous step so we perform some steps again
data <- read.csv("./sources/raw/dataset.csv", stringsAsFactors = F)
colnames(data) <- c('user_id', 'text', 'timestamp', 'screenname', 'location', 'timeline')
data$timeline <- NULL
data <- as_tibble(data)

indices <- c(9362:23213)
data[indices,'text'] <- data[indices, 'user_id']
data[indices, c('user_id', 'screenname', 'location')] <- NA
data[indices, c('timestamp')] <- '2020-03-13'

data$text <- topic_text
data <- data[!duplicated(data$text),]
write_csv(data, './sources/cleaned/dataset_topics_removed.csv')

