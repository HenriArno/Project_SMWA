# loading packages --------------------------------------------------------
rm(list=ls())
library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(rtweet, httr,tidyverse,wordcloud, tm, topicmodels, tidytext, textclean, fastDummies, ggplot2)


# loading data ------------------------------------------------------------
data <- as_tibble(read.csv("./sources/cleaned/dataset_cleaned.csv", stringsAsFactors = F))
topic_data <- as_tibble(read.csv("./sources/predictors/topic_models.csv", stringsAsFactors = F)) %>% select(-c('text', 'user_id'))
sentiment_dict_data <- as_tibble(read.csv("./sources/predictors/sentiment_dict.csv", stringsAsFactors = F))%>% select(-c('text', 'user_id', 'location', 'timestamp'))
sentiment_day_data <- as_tibble(read.csv("./sources/predictors/sentiment_dict_day.csv", stringsAsFactors = F))
sentiment_sentimentr_data1 <- as_tibble(read.csv("./sources/predictors/sentiment_sentimentr_1.csv", stringsAsFactors = F))%>% 
  select(-c('text', 'user_id', 'timestamp', 'screenname', 'location'))
sentiment_sentimentr_data2 <- as_tibble(read.csv("./sources/predictors/sentiment_sentimentr_2.csv", stringsAsFactors = F))%>% 
  select(-c('text', 'user_id', 'timestamp', 'screenname', 'location'))
dependent_data <- as_tibble(read.csv("./sources/raw/cancellations.csv"))



# Create basetable --------------------------------------------------------
#add missing user ID's for topic data
index <- enframe(data$rowid, name=NULL, value = "rowid")
topic_data <- left_join(index, topic_data, by = "rowid") %>% replace(is.na(.), 0)

#try left joins
basetable <- full_join(data, topic_data, by="rowid") %>% 
  full_join(sentiment_dict_data, by="rowid")%>%
  full_join(sentiment_sentimentr_data2, by="rowid")

rm(data,sentiment_dict_data, sentiment_sentimentr_data1, sentiment_sentimentr_data2, topic_data)


# recode timestamp variable -> we can't convert to POSIXct type due to length
basetable$timestamp <- substr(basetable$timestamp, 1, 10)
basetable <- basetable %>% arrange(., timestamp)
# removal of minor amount of wrongly inputed data
basetable <- basetable %>% drop_na(timestamp)
basetable <- basetable[-c(1:1567,168314:168345),]

# add dependent and average sentiment variable
dependent_data$timestamp <- as.POSIXct(dependent_data$day, format = "%d/%m/%Y")
dependent_data$day <- NULL
dependent_data$timestamp <- as.character(dependent_data$timestamp)

sentiment_day_data$timestamp <- unique(dependent_data$timestamp)[1:nrow(sentiment_day_data)]

basetable <- basetable %>% 
  merge(., sentiment_day_data, by = 'timestamp') %>%
  merge(., dependent_data, by = 'timestamp')

rm(dependent_data, sentiment_day_data, index)


# Turn timestamp into numeric variable
basetable$timestamp_numeric <- basetable$timestamp %>% as.factor() %>% as.integer()


# rename columns and rearrange column order
basetable$dates <- NULL
basetable$day <- NULL
basetable$day.x <- NULL
basetable$day.y <- NULL
basetable$negations.y <- NULL
basetable$rowid <- NULL

relevant_col <- c('text', 'timestamp', 'timestamp_numeric' ,'user_id', 'screenname.x', 'location', 'sentiment',
                  'ave_sentiment', 'negations', 'best_topic', 'best_topic_gamma', 
                  'topic_2_dummy', 'topic_3_dummy', 'topic_4_dummy', 'topic_1_gamma', 'topic_2_gamma',
                  'topic_3_gamma', 'topic_4_gamma','word_count', 'cancelled.flights')
basetable <- basetable[relevant_col]
rm(relevant_col)
colnames(basetable) <- c('text', 'timestamp', 'timestamp_numeric','user_id', 'screenname', 'location', 'sentiment_dict', 'sentimentr',
                         'sentiment_daily_avg','best_topic', 'best_topic_gamma', 
                         'topic_2_dummy', 'topic_3_dummy', 'topic_4_dummy', 'topic_1_gamma', 'topic_2_gamma',
                         'topic_3_gamma', 'topic_4_gamma','sentimentr__wordc', 'cancellations')


# Create additional dependent variabele (percentage change)
# This is nothing but a rescaling/shift in the data

# source: (https://www.quora.com/How-many-airplanes-fly-each-day-in-the-world)
# Note that this is a conservative approximation
absolute_number_approx <- 170000

# source: (https://www.bts.gov/newsroom/december-2016-airline-on-time-performance)
# baseline: 2016
percentage_cancelled_approx <- 0.0117

# Create the variable
basetable$percentage_change <- round((basetable$cancellations / absolute_number_approx) - percentage_cancelled_approx, 4)

# Write basetable to CSV --------------------------------------------------
basetable %>% write.csv(., "./sources/cleaned/basetable.csv")

# Read the basetable 
basetable <- read.csv("./sources/cleaned/basetable.csv")

ggplot(data = basetable, aes(timestamp, cancellations)) +
  geom_point() +
  geom_line()


