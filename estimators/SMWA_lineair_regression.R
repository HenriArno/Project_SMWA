# Set script -----------------------------------------------------------
rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")
#Load the required packages to perform topic modeling
if (!require("pacman")) install.packages("pacman") ; require("pacman")
if (!require("fastDummies")) install.packages("fastDummies") ; require("fastDummies")
if (!require("GGally")) install.packages("GGally") ; require("GGally")
if (!require("caTools")) install.packages("caTools") ; require("caTools")
p_load(rtweet, httr,tidyverse,wordcloud, tm, topicmodels, tidytext, textclean, fastDummies, ggplot2, GGally, caTools)

# Read in data ------------------------------------------------------------
basetable <- read.csv("./sources/cleaned/basetable.csv")
attach(basetable)


# Adjust basetable --------------------------------------------------------
basetable <- fastDummies::dummy_cols(basetable, 'timestamp', remove_first_dummy = T)
?fastDummies::dummy_cols()
basetable$X <- NULL
basetable$text <- NULL
basetable$timestamp <- NULL
basetable$user_id <- NULL
basetable$screenname <- NULL
basetable$location <- NULL
basetable$best_topic <- NULL
basetable$best_topic_gamma <- NULL
basetable$percentage_change <- NULL
basetable$sentimentr_1_wordc <- NULL
basetable$sentimentr_2_wordc <- NULL

# Some exploratory analysis -----------------------------------------------
relevant <- c('cancellations', 'sentiment_dict' , 'sentimentr_1' , 
              'sentiment_dict_daily_avg' , 'topic_1_dummy' , 'topic_2_dummy' ,'topic_3_dummy' , 'topic_4_dummy' ,
              'topic_1_gamma' , 'topic_2_gamma' , 'topic_3_gamma' , 'topic_4_gamma')

#ggpairs(basetable[relevant])

# Linear regression model -------------------------------------------------

# Create train and test set
set.seed(100)

sample <- sample.split(basetable, SplitRatio = 0.8)
train <- subset(basetable, sample == T)
test <- subset(basetable, sample == F)

# Create model
model_OLS_1 <- lm(cancellations ~ sentiment_dict + sentimentr_2 + 
                  sentiment_dict_daily_avg + topic_1_dummy + topic_2_dummy +topic_3_dummy + topic_4_dummy +
                  topic_1_gamma + topic_2_gamma + topic_3_gamma + topic_4_gamma , data = train)

cor(basetable)

model_OLS_2 <- lm(cancellations ~ sentiment_dict + sentimentr_2 + 
                  sentiment_dict_daily_avg + topic_1_dummy + topic_2_dummy +topic_3_dummy + topic_4_dummy +
                  topic_1_gamma + topic_2_gamma + topic_3_gamma + topic_4_gamma + 
                  `timestamp_2020-03-13` + `timestamp_2020-03-14` +
                  `timestamp_2020-03-15` + `timestamp_2020-03-16` +
                  `timestamp_2020-03-17` + `timestamp_2020-03-18` +
                  `timestamp_2020-03-19` + `timestamp_2020-03-20` +
                  `timestamp_2020-03-21` + `timestamp_2020-03-22` +
                  `timestamp_2020-03-23` + `timestamp_2020-03-24`, data = train)

summary(model_OLS_1)
summary(model_OLS_2)


# Make estimations for test set
test$predictions_1 <- predict(model_OLS_1, test)
test$predictions_2 <- predict(model_OLS_2, test)



# Some performance measures
actuals_preds <- test[c('cancellations', 'predictions')]
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predictions - actuals_preds$cancellations))/actuals_preds$cancellations)  
# R^2 en mean squared error

