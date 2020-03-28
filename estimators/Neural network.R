
# Neural network regression -----------------------------------------------
#In this script we will use a neural network rather than a linear regression to estimate the number of cancelled flights

# setting working directory -----------------------------------------------

rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#sets wd to root of the directory (one level up from the file location)
setwd("..")

# Load Packages-----------------------------------------------
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse, caret, nnet)

#import basetable-----------------------------------------------
basetable <- read.csv('./sources/cleaned/basetable.csv', header=T)
#Selecting relevant columns and recoding datetime to character for dummy coding later

basetable <- fastDummies::dummy_cols(basetable, 'best_topic')
basetable <- basetable %>% select(c("sentiment_dict", "sentimentr", 
                                    "percentage_change","best_topic_1", 'best_topic_2',
                                    "best_topic_3","best_topic_4","timestamp_numeric", "topic_1_gamma",
                                    "topic_2_gamma", 'topic_3_gamma','topic_4_gamma' ,"cancellations")) %>% 
  drop_na()

# Random sampling
samplesize = 0.75 * nrow(basetable)
set.seed(80)
index = sample( seq_len ( nrow ( basetable ) ), size = samplesize )

# Create training and test set
datatrain = basetable[ index, ]#Training set is used to find the relationship between dependent and independent variables
datatrain_2 = datatrain[,-8]
datatest = basetable[ -index, ] # test set assesses the performance of the model
datatest_2 = datatest[,-8]
# Implementing neural network ---------------------------------------------
#using caret we will set the hyperparameters of the neural network: decay and size

#set starting grid
mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6))
#get max value dependent variable
max <- max(basetable$cancellations)
#set neural network

nnetfit_1 <- train(percentage_change~. -cancellations, data = datatrain, method="nnet", maxit=1000, tuneGrid=mygrid, trace=F) 
nnetfit_2 <- train(percentage_change~. -cancellations, data = datatrain_2, method="nnet", maxit=1000, tuneGrid=mygrid, trace=F) 

print(nnetfit_1)
print(nnetfit_2)

preds_1 <- predict(nnetfit_1, datatest)
preds_2 <- predict(nnetfit_2, datatest)
datatest$pred_1 <- preds_1
datatest$pred_2 <- preds_2

#calculate mape
MAPE_1 <- mean(abs((datatest$percentage_change-preds_1)/datatest$percentage_change) * 100)
MAPE_2 <- mean(abs((datatest$percentage_change-preds_2)/datatest$percentage_change) * 100)

