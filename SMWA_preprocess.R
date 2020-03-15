# setwd and load packages -------------------------------------------------
rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Load Packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(httr,rtweet,tidyverse,textclean, textstem, sentimentr, lexicon, maps, dplyr)


# create tibble -----------------------------------------------------------
data <- read.csv("dataset.csv", stringsAsFactors = F)
colnames(data) <- c('user_id', 'text', 'timestamp', 'screenname', 'location', 'timeline')
data$timeline <- NULL
data <- as_tibble(data)

# impute missing data ---------------------------------------------------------

# adjust part of datadrame from row 9.362 untill 23.213 (due to bug in scrape function) 
indices <- c(9362:23213)
data[indices,'text'] <- data[indices, 'user_id']
data[indices, c('user_id', 'screenname', 'location')] <- NA
data[indices, c('timestamp')] <- '2020-03-13'
  






# Preprocess data ---------------------------------------------------------
############ WERKT NOG NIET VANF HIER ##############



text <- data$text
text 

text <- iconv(text, from = "latin1", to = "ascii", sub = "byte")


#Clean the rest of the posts
cleanText <- function(text) {
  clean_texts <- text %>%
    # gsub("<.*>", "", .) %>% # remove remainig emojis
    gsub("&amp;", "", .) %>% # remove &
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("@\\w+", "", .) %>% # remove at people replace_at() also works
    #gsub("(?:\\s*#\\w+)+\\s*$", "", .) %>% #remove hashtags in total
    gsub('#', "", .) %>% #remove only hashtag 
    gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("http\\w+", "", .) %>% # remove html links replace_html() also works
    # gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    #gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    tolower
  return(clean_texts)
}

text_clean <- cleanText(text) %>% replace_emoji() %>% replace_emoticon() %>% replace_contraction() %>%
  replace_internet_slang() %>% replace_kern() %>% replace_word_elongation()

#Finally, apply lemmatization with the textstem package
#First, you create a dictionary frm the text
#For large corpora you can use built-in dictionaries
lemma_dictionary_hs <- make_lemma_dictionary(text_clean,
                                             engine = 'hunspell')
text_clean <- lemmatize_strings(text_clean, dictionary = lemma_dictionary_hs)



