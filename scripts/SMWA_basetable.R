rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# loading packages --------------------------------------------------------
if (!require("pacman")) install.packages("pacman") ; require("pacman")
# loading data ------------------------------------------------------------
data <- read.csv("./sources/cleaned/dataset_cleaned.csv")
topic_data <- read.csv("./sources/predictors/topic_models.csv")
sentiment_data <- read.csv("./sources/predictors/sentiment.csv")
RF_sentiment_data <- read.csv("./sources/predictors/RF_sentiment.csv")
dependent_data <- read.csv("./sources/raw/cancellations.csv")
# Create basetable --------------------------------------------------------



# Write basetable to CSV --------------------------------------------------
basetable %>% write.csv(., "./sources/cleaned/basetable.csv")
