
# Random Forest Approach to Sentiment -------------------------------------

# In this script we will firstly make a dataset based on the positive and negative emojis present in our raw
# data. Next we will train a Random Forest model to identify sentiment and then apply said model to the rest
# of our data.


# Setting wd and clearing environmnet -------------------------------------

rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

# loading required packages -----------------------------------------------

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse,Unicode,tm, rvest, rtweet, stringr)

# Reading in data ---------------------------------------------

#reading in dataset as tibble
data <- read.csv("./sources/raw/dataset.csv", stringsAsFactors = F)
colnames(data) <- c('user_id', 'text', 'timestamp', 'screenname', 'location', 'timeline')
data$timeline <- NULL
data <- as_tibble(data)
#data <- data%>% slice(1:200)

# adjust part of datadrame from row 9.362 untill 23.213 (due to bug in scrape function) 
indices <- c(9362:23213)
data[indices,'text'] <- data[indices, 'user_id']
data[indices, c('user_id', 'screenname', 'location')] <- NA
data[indices, c('timestamp')] <- '2020-03-13'
rm(indices)
# Extract text from tibble
text <- data %>% select(text)

#transforming data to data we can work with
text <- text %>% mutate(text = iconv(text, from = "latin1", to = "ascii", sub = "byte"))



# constructing emoji dictionary -------------------------------------------

#I used the code linked to this article: https://www.r-bloggers.com/emojis-analysis-in-r/
#It constructs a dictionary for emojis 


# read in emoji dictionary
# I used to get the dictionary from Felipe: https://github.com/felipesua
# but he put it down, so I uploaded the csv file to my github profile: 
# https://raw.githubusercontent.com/today-is-a-good-day/emojis/master/emojis.csv
# input your custom path to file
emDict_raw <- read.csv2("./sources/raw/emojis.csv") %>% 
  select(description = EN, r_encoding = ftu8, unicode)

# plain skin tones
skin_tones <- c("light skin tone", 
                "medium-light skin tone", 
                "medium skin tone",
                "medium-dark skin tone", 
                "dark skin tone")

# remove plain skin tones and remove skin tone info in description
emDict <- emDict_raw %>%
  # remove plain skin tones emojis
  filter(!description %in% skin_tones) %>%
  # remove emojis with skin tones info, e.g. remove woman: light skin tone and only
  # keep woman
  filter(!grepl(":", description)) %>%
  mutate(description = tolower(description)) %>%
  mutate(unicode = gsub(' [A-z ]*', '' , unicode)) %>%
  mutate(unicode = as.u_char(unicode))
# all emojis with more than one unicode codepoint become NA 

matchto <- as.character(emDict$r_encoding)
description <- emDict$description

#the next part scrapes the results of a paper which adds valence scores to emojis
# reference website
url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"

# get emoticons
emojis_raw <- url %>%
  read_html() %>%
  html_table() %>%
  data.frame() %>%
  select(-Image.twemoji., -Sentiment.bar.c.i..95..)
names(emojis_raw) <- c("char", "unicode", "occurrences", "position", "negative", 
                       "neutral", "positive", "sentiment_score", "description", 
                       "block")
# change numeric unicode to character unicode to be able to match with emDict 
emojis <- emojis_raw %>%
  mutate(unicode = as.u_char(unicode)) %>%
  mutate(description = tolower(description)) 


# merge with emDict to get encoding
emojis_merged <- emojis %>%
  merge(emDict, by = "unicode")
# emojis %>% filter(!unicode %in% emDict$unicode) %>% View
# we lose 137 emojis that are not in emDict and for which we don't have an R encoding
# but they seem to be black and white emojis not too often used in social media anyways

#construct dictionary based on sourced information from code above
dict_full <- emojis_merged %>% select(sentiment_score, unicode, description.x, r_encoding, char)
dict <- dict_full %>% select(sentiment_score, r_encoding)
#delete remainders unused variables
rm(emDict, emDict_raw, emojis, emojis_merged, emojis_raw, description, matchto, skin_tones, url)



# Construct labeled dataset based on emoji --------------------------------------------------

#firstly we make a regex to select emojis on their UTF-8 representation
emoji_regex <- paste0(dict$r_encoding, collapse="|")

#initialise list to store scores in

score <- numeric((dim(text)[1]))
#initialise tibble to store text and fitting valence score
scores <- tibble() %>% add_column(text =NA) %>% add_column(score=NA)

for (i in 1:length(score)){
  
  #select entry
  entry <- (text%>% slice(i))[[1]]
  
  #select the emojis in the entry
  emojis <- str_extract_all(entry, emoji_regex)
  
  #find the positions of the words in the Tweet in the dictionary
  m <- match(emojis, dict$r_encoding)
  
  #which words are present in the dictionary?
  present <- !is.na(m)
  #tweetsplit[present]
  
  #of the words that are present, select their valence
  wordvalences <- dict$sentiment_score[m[present]]
  
  #compute the mean valence of the tweet
  score[i] <- mean(wordvalences, na.rm=TRUE)
  
  #handle the case when none of the words is in the dictionary
  if (is.na(score[i])) score[i] <- 0 
  else {
    score[i] <- score[i]
    scores <- scores %>% add_row(text = entry, score = score[i])
    }
}

scores <- scores[!duplicated(scores$text),]


#export text and valence scores to use later on (speed up next part by simply loading in that part)
write.csv(scores, "./sources/cleaned/training set.csv")

####################################################################################################
#####FROM HERE ON IN YOU CAN RUN THE CODE AS A STANDALONE FILE IN ORDER TO SPEED UP PROCESS TIME####
####################################################################################################


# constructing training set --------------------------------------
rm(list = ls())
#load packages
library(pacman)
p_load(SnowballC, slam, tm, RWeka, Matrix)
#read in data
data <- read.csv("./sources/cleaned/training set.csv")

#set label
data$score <- as.factor(data$score)
data <- data %>% select(-X)

#remove NA values
data <- data %>% drop_na()
#set seed for reproducablitiy
set.seed(1000) 




# Make our training corpus
corpus <- Corpus(VectorSource(data$text))

# Make a N-grams for train and test
# We will restrict to onegrams, but it can be adapted to N-grams

# Training
# We use a new function: NgramTokenizer(), which is further wrapped in the custom Tokenizer function
# Here, you can specify which degree of n-gram you want to include
# mindegree = 1 and maxdegree = 3 will include onegrams, bigrams and trigrams. 
# Next, we use this function in the control argument of the DocumentTermMatrix

Tokenizer <- function(x) NGramTokenizer(x, 
                                        Weka_control(min = mindegree, 
                                                     max = maxdegree))
dtm <- DocumentTermMatrix(corpus, control = list(tokenize = Tokenizer,
                                                             weighting = function(x) weightTf(x),
                                                             RemoveNumbers=TRUE,
                                                             removePunctuation=TRUE,
                                                             stripWhitespace= TRUE))


# Convert term document matrices to common sparse matrices to apply efficient SVD algorithm
# i are the row indices and j the column indices, v the values
dtm.to.sm <- function(dtm) {
  sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))
}

sm <- dtm.to.sm(dtm)

# 4. Apply Singular Value Decomposition
# SVD will help to reduce this to a selected number of terms
# Note that we implemented an approximation with the package irlba, 
# since the 'normal' svd gets stuck with very large datasets

p_load(irlba)

# Set k to a high number (20 in our case)
trainer <- irlba(t(sm), nu=20, nv=20)
# str(trainer)
# We are interested in the V
# str(trainer$v)

# 5. Modeling and evaluation: Random forest

p_load(randomForest, AUC, caret)



#apply random forest fit after setting label
y_train <- as.numeric(sub(",", ".", data$score))
#Random Forest Fit
RF <- randomForest(x = as.data.frame(trainer$v), y = y_train, ntree = 1000)

#assess performance of fit
print(RF)
#pretty low variance explained in this fit

#look at the mean squared error, using the OOB entries as test set
mean(RF$mse)
#inspect the pseudo R-squared, formula:(1 - mse / Var(y))
mean(RF$rsq)



# Random forest with categorical labels -----------------------------------
#In the next part we will recode the scores into a positive, negative or neutral variable and see the impact
#this has on our Random Forest model


# recoding label ----------------------------------------------------------

#store label as a vector
label <-as.numeric(sub(",", ".", data$score))

#recode label to category in data
data <- data %>% mutate(cat = cut(label, breaks =c(-Inf,0,0.0000001, Inf), right = F, labels = c("negative", "neutral", "positive")))%>% select(-score) 

#apply RF
RF <- randomForest(x=as.data.frame(trainer$v),y= as.factor(data$cat),
                   importance=TRUE,ntree=1000)
preds <- RF$predicted
error <- RF$err.rate
confusionmatrix <- RF$confusion
# Calculate AUC

#We can see that although the model will be quite efficient at detecting positive statements, negative statements
#might pose an issue due to the lower frequency of negative emojis present in our dataset



predict(RF, data$text)
