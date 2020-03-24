# loading packages --------------------------------------------------------
rm(list=ls())
library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
if (!require("pacman")) install.packages("pacman") ; require("pacman")


# loading data ------------------------------------------------------------
data <- read.csv("./sources/cleaned/dataset_cleaned.csv", stringsAsFactors = F)
topic_data <- read.csv("./sources/predictors/topic_models.csv", stringsAsFactors = F)
sentiment_dict_data <- read.csv("./sources/predictors/sentiment_dict.csv", stringsAsFactors = F)
sentiment_day_data <- read.csv("./sources/predictors/sentiment_dict_day.csv", stringsAsFactors = F)
sentiment_sentimentr_data1 <- read.csv("./sources/predictors/sentiment_sentimentr_1.csv", stringsAsFactors = F)
sentiment_sentimentr_data2 <- read.csv("./sources/predictors/sentiment_sentimentr_2.csv", stringsAsFactors = F)
dependent_data <- read.csv("./sources/raw/cancellations.csv")
#RF_sentiment_data <- read.csv("./sources/predictors/RF_sentiment.csv", stringsAsFactors = F)


# Create basetable --------------------------------------------------------

rm(basetable)
# merge the datasets
basetable <- sentiment_dict_data %>% 
  merge(sentiment_sentimentr_data1, by = 'text') %>%
  merge(sentiment_sentimentr_data2, by = 'text') %>%
  merge(topic_data, by = 'text')

rm(data,sentiment_dict_data, sentiment_sentimentr_data1, sentiment_sentimentr_data2, topic_data)

# remove redundant columns
basetable$user_id.x <- NULL
basetable$timestamp.x <- NULL
basetable$screenname.x <- NULL
basetable$location.x <- NULL
basetable$user_id.y <- NULL
basetable$timestamp.y <- NULL
basetable$screenname.y <- NULL
basetable$location.y <- NULL

# recode timestamp variable -> we can't convert to POSIXct type due to length
basetable$timestamp <- substr(basetable$timestamp, 1, 10)
unique(basetable$timestamp)
basetable <- basetable %>% arrange(., timestamp)
# removal of minor amount of wronglt inputed data
basetable <- basetable[-c(1:633),]


# add dependent and average sentiment variable
# hardcoding to get basetable quickly for prediction
# Will be changed soon
dates <- unique(basetable$timestamp)[1:13]
dependent_data <- dependent_data[5:17,]
dependent_data$timestamp <- dates

dates[14] = "2020-03-24"
sentiment_day_data$timestamp <- dates

basetable <- basetable %>% 
  merge(., sentiment_day_data, by = 'timestamp') %>%
  merge(., dependent_data, by = 'timestamp')

rm(dependent_data, sentiment_day_data)

# rename columns and rearrange column order
basetable$dates <- NULL
basetable$day <- NULL
basetable$day.x <- NULL
basetable$day.y <- NULL
basetable$negations.y <- NULL

basetable <- basetable[c('text', 'timestamp', 'user_id', 'screenname', 'location', 'sentiment', 'ave_sentiment.x',
                         'ave_sentiment.y', 'negations.x', 'best_topic', 'best_topic_gamma', 'topic_1_dummy', 
                         'topic_2_dummy', 'topic_3_dummy', 'topic_4_dummy', 'topic_1_gamma', 'topic_2_gamma',
                         'topic_3_gamma', 'topic_4_gamma','word_count.x','word_count.y', 'cancelled.flights')]
  
colnames(basetable) <- c('text', 'timestamp', 'user_id', 'screenname', 'location', 'sentiment_dict', 'sentimentr_1',
                         'sentimentr_2', 'sentiment_dict_daily_avg', 'best_topic', 'best_topic_gamma', 'topic_1_dummy', 
                         'topic_2_dummy', 'topic_3_dummy', 'topic_4_dummy', 'topic_1_gamma', 'topic_2_gamma',
                         'topic_3_gamma', 'topic_4_gamma','sentimentr_1_wordc','sentimentr_2_wordc', 'cancellations')
# Write basetable to CSV --------------------------------------------------
basetable %>% write.csv(., "./sources/cleaned/basetable.csv")


