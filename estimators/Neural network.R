
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
basetable <- basetable %>% select(c("sentiment_dict", "sentimentr_2", 
                                    "percentage_change", "topic_1_dummy","topic_2_dummy",
                                    "topic_3_dummy","topic_4_dummy","timestamp", "cancellations")) %>% 
  drop_na()

# Random sampling
samplesize = 0.75 * nrow(basetable)
set.seed(80)
index = sample( seq_len ( nrow ( basetable ) ), size = samplesize )

# Create training and test set
datatrain = basetable[ index, ] #Training set is used to find the relationship between dependent and independent variables
datatest = basetable[ -index, ] # test set assesses the performance of the model

# Implementing neural network ---------------------------------------------
#using caret we will set the hyperparameters of the neural network: decay and size

#set starting grid
mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6))
#get max value dependent variable
#max <- max(basetable$cancellations)
#set neural network
nnetfit <- train(percentage_change~. -cancellations, data = datatrain, method="nnet", maxit=1000, tuneGrid=mygrid, trace=F) 
print(nnetfit)

preds <- predict(nnetfit, datatest)
#calculate mape
MAPE <- mean(abs((datatest$percentage_change-preds)/datatest$percentage_change) * 100)
