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
model_OLS <- lm(cancellations ~ sentiment_dict + sentimentr_1 + sentimentr_2 + 
                  sentiment_dict_daily_avg + topic_1_dummy + topic_2_dummy +topic_3_dummy + topic_4_dummy +
                  topic_1_gamma + topic_2_gamma + topic_3_gamma + topic_4_gamma, data = train)

summary(model_OLS)


# Make estimations for test set
test$predictions <- predict(model_OLS, test)


# Some performance measures
actuals_preds <- test[c('cancellations', 'predictions')]
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predictions - actuals_preds$cancellations))/actuals_preds$cancellations)  


