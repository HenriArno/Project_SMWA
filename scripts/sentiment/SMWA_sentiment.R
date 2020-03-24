# Sentiment analysis ------------------------------------------------------
rm(list=ls())

# loading required packages -----------------------------------------------
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(SnowballC, slam, tm, RWeka, Matrix, readr, tidyverse, lubridate)

#import dataset
library(readr)
dataset <- read_csv("./sources/cleaned/dataset_cleaned.csv", col_names = TRUE)
#dataset <- dataset %>% slice(., 1:200)

#order dataset by timestamp column
dataset<-dataset[order(dataset$timestamp),]  

#test sample to run code faster
#dataset <- sample_n(dataset, 40000)

text<- dataset %>% select(text)
created <- dataset%>% select(timestamp)

###################### Load dictionary #####################

#Reload the dictionary such that everything is between 0 and 9
dictionary <- read_csv("./sources/raw/dictionary.csv")

#1) Put not in front and new valence scores
dictionary_new <- dictionary[,c('Word','VALENCE')]

dictionary_new$Word <- paste('not', dictionary$Word)
dictionary_new$VALENCE <- abs(dictionary$VALENCE-10)-dictionary$VALENCE

#2)rbind the new and the old dictionary
dictionary_negation <- rbind(dictionary[,c('Word','VALENCE')], 
                             dictionary_new)
#remove dictionaries we won't use
rm(dictionary_new, dictionary)

#3)run all unigrams and bigrams through the dictionary
score_negation <- numeric(dim(text)[1])


for (i in 1:length(score_negation)){
  #define entry
  entry = (text %>% slice(i))[[1]]
  
  #Split up the tweet in words
  unigrams <- strsplit(entry,split=" ")[[1]] 
  
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

#some results
mean(score_negation) 
sd(score_negation) 
hist(score_negation)


#using lubridate, add a column which has the day of the week as string
breaksday <- created%>% mutate(day(timestamp))
breaksmonth<-created%>%mutate(month(timestamp))

#tibble -> vector
days<-deframe(breaksday)
months<-deframe(breaksmonth)


#Now lets make the plots
attributes(created)$tzone <- "CET"

#average and stdev per day
negations_sd <- aggregate(score_negation,by=list(paste(months,days)),sd)$x
negations <- aggregate(score_negation,by=list(paste(months,days)),mean)$x
lim <- max(abs(negations)) #we can use this for graph 
#view(negations)

#Plot sentiment by day
plot(1:length(negations), 
     negations, 
     xaxt="n",
     type="o",
     ylab="Valence",
     xlab="day",
     main="Sentiment by day", 
     #ylim=c(-lim,lim))
     ylim=c(-2,2))
#length(unique(days))

#add labels on x-as and add stdev
axis(1,at=1:length(negations), 
     labels=unique(substr(created$timestamp,6,10)))
lines(negations+negations_sd,col="green")
lines(negations-negations_sd,col="green")



# Write table -------------------------------------------------------------
final_table <- dataset
final_table$sentiment <- score_negation
avg_day <- as.data.frame(negations)
write_csv(final_table, './sources/predictors/sentiment_dict.csv')
write_csv(avg_day, './sources/predictors/sentiment_dict_day.csv')
