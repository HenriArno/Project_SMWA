
# Sentiment analysis ------------------------------------------------------
rm(list=ls())

#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# loading required packages -----------------------------------------------

if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(SnowballC, slam, tm, RWeka, Matrix, readr, tidyverse, lubridate)


#import dataset and add column names to extract the raw text
#dataset <- read_csv("dataset_cleaned.csv")
library(readr)
dataset <- read_csv("./sources/raw/dataset.csv", col_names = FALSE)
#View(dataset)
dataset=dataset %>% 
  rename(
    user_id=X1,
    text=X2,
    timestamp=X3,
    screenname=X4,
    location=X5,
  )

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
#Let's look at the compare the results
mean(score_negation) 
sd(score_negation) 
hist(score_negation)

####different approach######

#using lubridate, add a column which has the day of the week as string
breaksday <- created%>% mutate(day(timestamp))
breaksmonth<-created%>%mutate(month(timestamp))
days<-deframe(breaksday)
months<-deframe(breaksmonth)

#make tibble of the scores to join data together 
#scores <- enframe(score_negation, name=NULL)
#to_plot <- created%>% add_column(scores) %>% select(-timestamp)



#Now lets make the plots
#Group in days and take the average per day
#handle time zone
attributes(created)$tzone <- "CET"


#get days for tweets
#breaksday <- as.integer(cut(created, breaks="day"))
#breaksday <- created$timestamp$mday
#str(created$timestamp)
#time<-created$timestamp
#unclass(time)
#day(time)
#Compute mean
negations_sd <- aggregate(score_negation,by=list(paste(months,days)),sd)$x
negations <- aggregate(score_negation,by=list(paste(months,days)),mean)$x
lim <- max(abs(negations))
view(negations)

#Plot sentiment by time
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

axis(1,at=1:length(negations), 
     labels=unique(substr(created$timestamp,6,10)))
lines(negations+negations_sd,col="green")
lines(negations-negations_sd,col="green")
