
# Random Forest Approach to Sentiment -------------------------------------

# In this script we will firstly make a dataset based on the positive and negative emojis present in our raw
# data. Next we will train a Random Forest model to identify sentiment and then apply said model to the rest
# of our data.


# Setting wd and clearing environmnet -------------------------------------

rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# loading required packages -----------------------------------------------

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse,Unicode,tm, rvest, rtweet, stringr)

# Reading in data ---------------------------------------------

#reading in dataset as tibble
data <- read.csv("dataset.csv", stringsAsFactors = F)
colnames(data) <- c('user_id', 'text', 'timestamp', 'screenname', 'location', 'timeline')
data$timeline <- NULL
data <- as_tibble(data)

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
score <- numeric((dim(text))[1])
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

#apply text cleaning in order to make dataset ready for analysis(see preprocess file for full information)
p_load(httr,rtweet,tidyverse,textclean, textstem, sentimentr, lexicon, maps, dplyr)
text <- scores$text

cleanText <- function(text) {
  clean_texts <- text %>%
    gsub("&amp;", "", .) %>% # remove &
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("@\\w+", "", .) %>% # remove at people replace_at() also works
    gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    gsub('#', "", .) %>% #remove only hashtag 
    #gsub("<.*>", "", .) %>% # remove remainig emojis
    #gsub("(?:\\s*#\\w+)+\\s*$", "", .) %>% #remove hashtags in total
    #gsub("http\\w+", "", .) %>% # remove html links replace_html() also works
    tolower
  return(clean_texts)
}

# apply the cleaning functions and save the text in text_clean
text_clean <- cleanText(text) %>% replace_emoji() %>% replace_emoticon() %>% replace_contraction() %>%
  replace_internet_slang() %>% replace_kern() %>% replace_word_elongation()

# 3. Perform lemmatization
lemma_dictionary_hs <- make_lemma_dictionary(text_clean,
                                             engine = 'hunspell')
text_clean <- lemmatize_strings(text_clean, dictionary = lemma_dictionary_hs)

# store the cleaned text in the dataset and remove redundant tweets
scores$text <- text_clean
scores <- scores[!duplicated(scores$text),]


#export text and valence scores to use later on (speed up next part by simply loading in that part)
write.csv(scores, "./sources/cleaned/training set.csv")

####################################################################################################
#####FROM HERE ON IN YOU CAN RUN THE CODE AS A STANDALONE FILE IN ORDER TO SPEED UP PROCESS TIME####
####################################################################################################


# constructing training and test set --------------------------------------
rm(list = ls())
#load packages
library(pacman)
p_load(SnowballC, slam, tm, RWeka, Matrix)
#read in data
data <- read.csv("./sources/cleaned/training set.csv")

#set label
label <- as.factor(data$score)
#set seed for reproducablitiy
set.seed(1000) 
#divide up data in training and test set
ind <- sample(x = nrow(data), 
              size = nrow(data),
              replace = FALSE)
train <- data[1:floor(length(ind)*.60),]
test <- data[(floor(length(ind)*.60)+1):(length(ind)),]


# Make our training, test set corpora
corpus_train <- Corpus(VectorSource(train$text))
corpus_test <- Corpus(VectorSource(test$text))

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
dtm_train <- DocumentTermMatrix(corpus_train, control = list(tokenize = Tokenizer,
                                                             weighting = function(x) weightTf(x),
                                                             RemoveNumbers=TRUE,
                                                             removePunctuation=TRUE,
                                                             stripWhitespace= TRUE))
# Test
# Training and test set have to be prepared in the same way
dtm_test <- DocumentTermMatrix(corpus_test, control = list(tokenize = Tokenizer,
                                                           weighting = function(x) weightTf(x),
                                                           RemoveNumbers=TRUE,
                                                           removePunctuation=TRUE,
                                                           stripWhitespace= TRUE))


# Reform the test DTM to have the same terms as the training case 
# Remember that our test set should contain the same elements as our training dataset

prepareTest <- function (train, test) {
  Intersect <- test[,intersect(colnames(test), colnames(train))]
  diffCol <- dtm_train[,setdiff(colnames(train),colnames(test))]
  newCols <- as.simple_triplet_matrix(matrix(0,nrow=test$nrow,
                                             ncol=diffCol$ncol))
  newCols$dimnames <- diffCol$dimnames
  testNew<-cbind(Intersect,newCols)
  testNew<- testNew[,colnames(train)]
}

dtm_test <- prepareTest(dtm_train, dtm_test)

# Convert term document matrices to common sparse matrices to apply efficient SVD algorithm
# i are the row indices and j the column indices, v the values
dtm.to.sm <- function(dtm) {
  sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))
}

sm_train <- dtm.to.sm(dtm_train)
sm_test <- dtm.to.sm(dtm_test)

# 4. Apply Singular Value Decomposition
# SVD will help to reduce this to a selected number of terms
# Note that we implemented an approximation with the package irlba, 
# since the 'normal' svd gets stuck with very large datasets

p_load(irlba)

# Set k to a high number (20 in our case)
trainer <- irlba(t(sm_train), nu=20, nv=20)
# str(trainer)
# We are interested in the V
# str(trainer$v)
tester <- as.data.frame(as.matrix(sm_test) %*% trainer$u %*%  solve(diag(trainer$d)))

# 5. Modeling and evaluation: Random forest

p_load(randomForest, AUC, caret)

#apply random forest fit

RF <- randomForest(x = trainer$V, y = label, ntree = 1000)
preds <- predict(RF, as.data.frame(tester))[,2]

# Calculate AUC
auc(roc(preds,y_test))

# ROC curve
plot(roc(preds,y_test))

# Normally you should use cross-validation to see whether your results are robust
# The AUC is pretty good in this case, so you could now extrapolate this model to unseen comments

# Sometimes other performance measures are also reported (accuracy)
# To so so, let's make predict the labels with random forest
preds_lab <- predict(RF,tester,type = "response")

p_load("e1071")
#Now make a confusion matrix
xtab <- table(preds_lab, y_test)
confusionMatrix(xtab)


