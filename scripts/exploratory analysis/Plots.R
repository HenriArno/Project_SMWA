# loading packages --------------------------------------------------------
rm(list=ls())
library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(rtweet, httr,tidyverse,wordcloud, tm, topicmodels, tidytext, textclean, fastDummies, ggplot2)

basetable <- read.csv("./sources/cleaned/basetable.csv")


sentiment_data <- basetable %>%
  group_by(timestamp) %>%
  summarise(
    sentimentr = mean(sentimentr),
    sentiment_dict = mean(sentiment_dict),
    sentiment_daily_avg = mean(sentiment_daily_avg),
    best_topic_gamma = mean(best_topic_gamma),
    cancellations = mean(cancellations),
    percentage_change = mean(percentage_change)
  )
  
rm(basetable)

ggplot(sentiment_data, aes(timestamp, sentimentr)) + 
  geom_point()

ggplot(sentiment_data, aes(timestamp, sentiment_dict)) + 
  geom_point()

ggplot(sentiment_data, aes(timestamp, sentiment_daily_avg)) + 
  geom_point()

ggplot(sentiment_data, aes(timestamp, best_topic_gamma)) + 
  geom_point()

ggplot(sentiment_data, aes(timestamp, cancellations)) + 
  geom_point()

ggplot(sentiment_data, aes(timestamp, percentage_change)) + 
  geom_point()

