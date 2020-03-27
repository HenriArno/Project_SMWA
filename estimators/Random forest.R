
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
basetable <- basetable %>% select(c("sentiment_dict", "sentimentr_1","sentimentr_2", 
                                    "percentage_change", "topic_1_dummy","topic_2_dummy",
                                    "topic_3_dummy","topic_4_dummy","timestamp", "cancellations")) %>% 
                                      drop_na() #%>% slice(1:2000)
#set label as y and variables as tibble x
y <- basetable$cancellations
x <- basetable %>% select(-c("percentage_change", "cancellations"))



# fit Random Forest -------------------------------------------------------

RF <- randomForest(x,y, importance= T,mtry = 8, ntree = 1000)

#assess model accuracy
mean(RF$mse)
mean(RF$rsq)
print(RF)
preds <- RF$predicted
importance <- RF$importance
