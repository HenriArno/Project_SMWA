
# Estimation using Random Forest ------------------------------------------
# In this script we will use our sentiment variables to predict the percentage change in
# flights cancelled.


# setting working directory -----------------------------------------------

rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#sets wd to root of the directory (one level up from the file location)
setwd("..")

# Load Packages-----------------------------------------------
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, caret, randomForest)

#import basetable-----------------------------------------------
basetable <- read.csv('./sources/cleaned/basetable.csv', header=T)
basetable <- basetable %>% select(c("sentiment_dict", "sentimentr", 
                                    "percentage_change","topic_2_dummy",
                                    "topic_3_dummy","topic_4_dummy","timestamp_numeric", "topic_1_gamma",
                                    "topic_2_gamma", 'topic_3_gamma','topic_4_gamma' ,"cancellations")) %>% 
  drop_na()
#set label as y and variables as tibble x
y <- basetable$cancellations
x <- basetable %>% select(-c("percentage_change", "cancellations"))
x_2 <- basetable %>% select(-c("percentage_change", "cancellations", "timestamp_numeric"))


# fit Random Forest -------------------------------------------------------

RF <- randomForest(x,y, importance= T, mtry = 2, ntree = 100)
RF_2 <- randomForest(x_2,y, importance= T, mtry = 2, ntree = 100)

#assess model accuracy
mean(RF$mse)
mean(RF$rsq)
print(RF)
preds <- RF$predicted
importance <- as.data.frame(RF$importance) %>% arrange()

mean(RF_2$mse)
mean(RF_2$rsq)
print(RF_2)
preds_2 <- RF_2$predicted
importance_2 <- RF_2$importance
