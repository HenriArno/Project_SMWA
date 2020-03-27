# loading packages --------------------------------------------------------
rm(list=ls())
#library(rstudioapi)
#sets working directory to file directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("..")
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(rtweet, httr,tidyverse,wordcloud, tm, topicmodels, tidytext, textclean, fastDummies, ggplot2)


# loading data ------------------------------------------------------------
data <- read.csv("./sources/cleaned/dataset_cleaned.csv", stringsAsFactors = F)%>% arrange(user_id)
topic_data <- read.csv("./sources/predictors/topic_models.csv", stringsAsFactors = F)%>% arrange(user_id)
sentiment_dict_data <- read.csv("./sources/predictors/sentiment_dict.csv", stringsAsFactors = F)%>% arrange(user_id)
sentiment_day_data <- read.csv("./sources/predictors/sentiment_dict_day.csv", stringsAsFactors = F)%>% arrange(user_id)
sentiment_sentimentr_data1 <- read.csv("./sources/predictors/sentiment_sentimentr_1.csv", stringsAsFactors = F)%>% 
  mutate (user_id <- as.numeric(user_id))%>% arrange(user_id)
sentiment_sentimentr_data2 <- read.csv("./sources/predictors/sentiment_sentimentr_2.csv", stringsAsFactors = F)%>% 
  mutate (user_id <- as.numeric(user_id))%>% arrange(user_id)
dependent_data <- read.csv("./sources/raw/cancellations.csv")
#RF_sentiment_data <- read.csv("./sources/predictors/RF_sentiment.csv", stringsAsFactors = F)


#sentiment_day_data has no user_id !


# Create basetable --------------------------------------------------------


#set basetable in right format and order
sentiment_dict_data <- as_tibble(sentiment_dict_data)
sentiment_dict_data <- sentiment_dict_data %>% arrange(user_id)
sentiment_dict_data <- sentiment_dict_data %>% drop_na(user_id)


topic_data <- as_tibble(topic_data)
topic_data <- topic_data %>% arrange(user_id)
topic_data <- topic_data %>% drop_na(user_id)


sentiment_sentimentr_data2 <- as_tibble(sentiment_sentimentr_data2)
sentiment_sentimentr_data2$user_id <- as.numeric(sentiment_sentimentr_data2$user_id)
sentiment_sentimentr_data2 <- sentiment_sentimentr_data2 %>% arrange(user_id)
sentiment_sentimentr_data2 <- sentiment_sentimentr_data2 %>% drop_na(user_id)

#Create basetable

#overview of relevant columns
sdd <- colnames(sentiment_dict_data)
ssd2 <- colnames(sentiment_sentimentr_data2)
td <- colnames(topic_data)
sdd
ssd2 <- ssd2[1:2]
td <- td[3:12]

#add relevant columns


#basetable <- sentiment_dict_data %>%
  #add_column(sentiment_sentimentr_data2$word_count) %>% 
  #add_column(sentiment_sentimentr_data2$ave_sentiment)

for(name in td){
  print(name)
  #basetable <- basetable %>% add_column(topic_data$name)
  #basetable <- basetable %>% add_column(topic_data[,name])
  
  #dat werkt precies nie 
}

#hard code version (should do the same thing)

basetable <- sentiment_dict_data %>%
  add_column(sentiment_sentimentr_data2$word_count) %>% 
  add_column(sentiment_sentimentr_data2$ave_sentiment)%>%
  add_column(topic_data$best_topic)%>% 
  add_column(topic_data$best_topic_gamma)%>% 
  add_column(topic_data$topic_1_dummy)%>% 
  add_column(topic_data$topic_2_dummy)%>% 
  add_column(topic_data$topic_3_dummy)%>% 
  add_column(topic_data$topic_4_dummy)%>% 
  add_column(topic_data$topic_1_gamma)%>% 
  add_column(topic_data$topic_2_gamma)%>% 
  add_column(topic_data$topic_3_gamma)%>% 
  add_column(topic_data$topic_4_gamma)


#adjust the column names
colnames(basetable) <- c("user_id","text","timestamp","screenname","location","sentiment","word_count","ave_sentiment",
                         "best_topic","best_topic_gamma","topic_1_dummy","topic_2_dummy","topic_3_dummy","topic_4_dummy",
                         "topic_1_gamma","topic_2_gamma","topic_3_gamma","topic_4_gamma")

#topic data heeft niet evenveel rows als sdd en ssd2

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
basetable <- basetable %>% arrange(., timestamp)
# removal of minor amount of wrongly inputed data
basetable <- basetable[-c(1:633,124660:124666),]

# add dependent and average sentiment variable
# hardcoding to get basetable quickly for prediction
# Will be changed soon
dependent_data$timestamp <- as.POSIXct(dependent_data$day, format = "%d/%m/%Y")
dependent_data$day <- NULL
dependent_data$timestamp <- as.character(dependent_data$timestamp)

# STILL CHECK IF THE DATES CORRECTLY MATCH THE NEGATION VALUES!!!!!
sentiment_day_data$timestamp <- unique(dependent_data$timestamp)[1:nrow(sentiment_day_data)]

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

relevant_col <- c('text', 'timestamp', 'user_id', 'screenname', 'location', 'sentiment', 'ave_sentiment.x',
                  'ave_sentiment.y', 'negations', 'best_topic', 'best_topic_gamma', 'topic_1_dummy', 
                  'topic_2_dummy', 'topic_3_dummy', 'topic_4_dummy', 'topic_1_gamma', 'topic_2_gamma',
                  'topic_3_gamma', 'topic_4_gamma','word_count.x','word_count.y', 'cancelled.flights')
basetable <- basetable[relevant_col]
rm(relevant_col)


colnames(basetable) <- c('text', 'timestamp', 'user_id', 'screenname', 'location', 'sentiment_dict', 'sentimentr_1',
                         'sentimentr_2', 'sentiment_dict_daily_avg', 'best_topic', 'best_topic_gamma', 'topic_1_dummy', 
                         'topic_2_dummy', 'topic_3_dummy', 'topic_4_dummy', 'topic_1_gamma', 'topic_2_gamma',
                         'topic_3_gamma', 'topic_4_gamma','sentimentr_1_wordc','sentimentr_2_wordc', 'cancellations')


# Create additional dependent variabele (percentage change)
# This is nothing but a rescaling/shift in the data

# source: (https://www.quora.com/How-many-airplanes-fly-each-day-in-the-world)
# Note that this is a conervative approximation
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


