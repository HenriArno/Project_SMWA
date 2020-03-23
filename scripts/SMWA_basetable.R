rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# loading packages --------------------------------------------------------
if (!require("pacman")) install.packages("pacman") ; require("pacman")
# loading data ------------------------------------------------------------
data = read.csv("./sources/cleaned/dataset_cleaned.csv")
# write.csv("./sources/predictors/NAME.csv")
# read.csv("./sources/predictors/NAME.csv")