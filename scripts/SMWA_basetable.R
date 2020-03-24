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


# rename columns and rearrange column order
  
  
  
# Write basetable to CSV --------------------------------------------------
basetable %>% write.csv(., "./sources/cleaned/basetable.csv")


