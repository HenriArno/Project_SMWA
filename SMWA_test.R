# Test script 


library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.csv("dataset.csv")
