# Sentiment Analysis using Sentimentr package -----------------------------
#In this script we will analyse our cleaned tweets to check for sentiment using the sentiment r package

# Setting wd and clearing environmnet -------------------------------------

rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

# loading packages --------------------------------------------------------
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(httr,rtweet,tidyverse,textclean, textstem, sentimentr, lexicon)



# loading data ------------------------------------------------------------
data = read.csv("./sources/cleaned/dataset_cleaned.csv")
#slice data to make code run faster for debug (TEMPORARY)
#data <- data%>% slice(1:200)
#Select text to perform analysis on
text <- as.vector(data$text)


# perform sentiment analyis -----------------------------------------------

#get the sentiment
sentiment <- text %>% get_sentences()%>%sentiment_by()
#make matching index columns
sentiment$X <-  sentiment$element_id
data$X<-sentiment$element_id
#join everything in tibble to get a clear result
result <- sentiment%>%select(-c(element_id, sd))%>% merge(data, by = 'X') %>% select(-X)

# Sentiment with punctuation ----------------------------------------------
#We are not looking at punctuation in the previous case however, 
#which is why we will update the Valence shifters table here

#set question mark and exclamation mark to the amplifier class of valence shifter (=2) and update table
update <- data.frame(x=c('exclamation', 'question mark'), y = c(2,2))
valence_shifters_updated <-update_valence_shifter_table(key = hash_valence_shifters,
                                                        x = update)

#replace punctuation in text with their respective textual counterpart
text <- as.vector(data$text)
text <- text %>% 
  gsub('!', 'exclamation', .) %>%
  gsub('\\?', 'question mark', .)


#perform analysis
sentiment2 <- text %>% get_sentences()%>%sentiment_by(valence_shifters_dt = valence_shifters_updated)
#make matching index columns
sentiment2$X <-  sentiment2$element_id
data <- data%>%rowid_to_column( 'X')
#join everything in tibble to get a clear result
result2 <- sentiment2%>%select(-c(element_id, sd))%>% merge(data, by = 'X') %>% select(-X)


# Write csv file ----------------------------------------------------------
write_csv(result2, './sources/predictors/sentiment_sentimentr_2.csv')

