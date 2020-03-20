#sentiment

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(SnowballC, slam, tm, RWeka, Matrix)
#import dataset
library(readr)
dataset <- read_csv("~/Project_SMWA/dataset.csv", 
                    col_names = FALSE)
View(dataset)
colnames(dataset) <- c('user_id', 'text', 'created_at', 'screen_name', 'location', 'timeline')
text<- dataset %>% select(text)
created <- dataset%>% select(created_at)


#As you can see text is pretty dirty, so we need to perform preprocessing
View(text)


###################### Load dictionary #####################

#Reload the dictionary such that everything is between 0 and 9
dictionary <- read_csv("dictionary.csv")

#1) Put not in front and new valence scores
dictionary_new <- dictionary[,c('Word','VALENCE')]

dictionary_new$Word <- paste('not', dictionary$Word)
dictionary_new$VALENCE <- abs(dictionary$VALENCE-10)-dictionary$VALENCE

#2)rbind the new and the old dictionary
dictionary_negation <- rbind(dictionary[,c('Word','VALENCE')], 
                             dictionary_new)

dictionary_negation

#3)run all unigrams and bigrams through the dictionary

score_negation <- numeric(length(text))


for (i in 1:length(text)){
  
  #transform everything to lower case
  text <- tolower(text)
  
  #Split up the tweet in words
  unigrams <- strsplit(text[i],split=" ")[[1]] 
  
  #Split in bigrams
  bigram <- rbind(unigrams,c(unigrams[2:(length(unigrams))],""))
  bigrams <- paste(bigram[1,],bigram[2,])
  
  #find the positions of the unigrams in the Tweet in the dictionary
  m <- match(unigrams, dictionary_negation$Word)
  
  #which unigrams are present in the dictionary?
  present <- !is.na(m)
  
  #find the positions of the bigrams in the Tweet in the dictionary
  b <- match(bigrams, dictionary_negation$Word)
  
  #which bigramsS are present in the dictionary?
  bpresent <- !is.na(b)
  
  #4)Look up the valence scores of the unigrams and bigrams
  wordvalences <- dictionary_negation$VALENCE[c(b[bpresent],m[present])]
  
  #compute the mean valence of the tweet
  score_negation[i] <- mean(wordvalences, na.rm=TRUE)
  
  #handle the case when none of the words is in the dictionary
  #also center around 5 if the words are found
  if (is.na(score_negation[i])) score_negation[i] <- 0 else score_negation[i] <- score_negation[i]-5
  
}
#Let's look at the compare the results
mean(score_negation) ; mean(scoretweet)
sd(score_negation) ; sd(scoretweet)
hist(score_negation); hist(scoretweet)

#Correlation between both
cor(scoretweet, score_negation)

#Now lets make the plots
#Group in minutes and take the average per minute
#handle time zone
time <- as.POSIXct(created, format="%Y-%m-%d %H:%M:%S",tz="UTC")
attributes(time)$tzone <- "CET"
