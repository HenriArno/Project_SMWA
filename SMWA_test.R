# Test script 
rm(list=ls())


library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Load Packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(httr,rtweet,tidyverse,textclean, textstem, sentimentr, lexicon, maps, dplyr)


df <- read.csv("dataset.csv")
colnames(df) <- c('id', 'text', 'timestamp', 'user_name', 'loc', 'timeline')
df %>% distinct() %>% dim()
