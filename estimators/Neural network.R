
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
#deselecting unused columns (and slicing for now)
basetable <- basetable %>% select(-c("text", "user_id", "screenname", "location", "X", "timestamp")) %>% drop_na() %>% slice(1:2000)
#set label as y and variables as tibble x


# Implementing neural network ---------------------------------------------
#using caret we will set the hyperparameters of the neural network: decay and size

#set starting grid
mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6))
#set neural network
nnetfit <- train(cancellations~., data = basetable, method="nnet", maxit=1000, tuneGrid=mygrid, trace=F) 
print(nnetfit)
