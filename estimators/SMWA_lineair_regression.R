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
p_load(rtweet, httr,tidyverse,wordcloud, tm, topicmodels, tidytext, textclean, fastDummies, ggplot2, GGally)

# Read in data ------------------------------------------------------------
basetable <- read.csv("./sources/cleaned/basetable.csv")
attach(basetable)

# Some exploratory analysis -----------------------------------------------
relevant <- c('cancellations', 'sentiment_dict' , 'sentimentr_1' , 
              'sentiment_dict_daily_avg' , 'topic_1_dummy' , 'topic_2_dummy' ,'topic_3_dummy' , 'topic_4_dummy' ,
              'topic_1_gamma' , 'topic_2_gamma' , 'topic_3_gamma' , 'topic_4_gamma')

#ggpairs(basetable[relevant])

# Linear regression model -------------------------------------------------
#model_OLS <- lm(cancellations ~ sentiment_dict + sentimentr_1 + sentimentr_2 + 
#                  sentiment_dict_daily_avg + topic_1_dummy + topic_2_dummy +topic_3_dummy + topic_4_dummy +
#                  topic_1_gamma + topic_2_gamma + topic_3_gamma + topic_4_gamma, data = basetable)
#

# Without sentimentr_2 because this predictor hasn't been correctly defined yet
model_OLS <- lm(cancellations ~ sentiment_dict + sentimentr_1 + 
                  sentiment_dict_daily_avg + topic_1_dummy + topic_2_dummy +topic_3_dummy + topic_4_dummy +
                  topic_1_gamma + topic_2_gamma + topic_3_gamma + topic_4_gamma, data = basetable)

summary(model_OLS)




