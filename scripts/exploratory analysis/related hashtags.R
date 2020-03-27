################ Wordcloud on dataset #################


# loading required packages -----------------------------------------------

# Clean Workspace and setwd
rm(list=ls())

library(rstudioapi)
#sets working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

# Load Packages
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse,tm,wordcloud, wordcloud2, dplyr)

#extracting the text of the tweets in the dataset
dataset <- read.csv('./sources/raw/dataset.csv', header=F)
dataset <- dataset[1:20000,]
text <- dataset$V2
#create term-by-document matrix
content <- Corpus(VectorSource(text))

#We measure the number of unique words for a document in the corpus as follows:
unique_word_count <- function(data){
  content <- character(length(data))
  if (any(class(data) %in% c("VCorpus", "Corpus","SimpleCorpus"))) {
    for (i in 1:length(data)) content[i] <- data[[i]]$content
  } else {
    content <- data
  }
  uniquewords <- unique(unlist(sapply(
    strsplit(as.character(content)," "),
    unique)))
  length(uniquewords)
}
unique_word_count(content)

#Case conversion transforms all values to lower case
content <- tm_map(content, 
                  content_transformer(tolower))
unique_word_count(content)

#remove stopwords
forremoval <- stopwords('english')
head(forremoval)
content <-  tm_map(content, 
                   removeWords, 
                   c(forremoval))
unique_word_count(content)
#The number of words decreased

#Collapse multiple white space in a 
#single whitespace
content <-  tm_map(content, 
                   stripWhitespace)


#create term-by-document matrix
tdm <- TermDocumentMatrix(content)
m <- as.matrix(tdm)

#Count occurrences
v <- sort(rowSums(m),decreasing=TRUE)
d <- tibble(word = names(v),freq=v) #or a data.frame(word = names(v),freq=v) 

#filter hashtags
d <- filter(d, grepl(pattern="^#.*", d$word))

#remove hashtags already in use
hashtags <- (read.delim("./sources/raw/hashtags.txt", header = F))
hashtags <- lapply(hashtags$V1, tolower)
for(tag in hashtags){
  d <- d %>% filter(!grepl(tag, word))
}

#Order the data frame
d <- d %>% arrange(desc(freq))
#make copy and remove hashtags for wordcloud processing
tags <- d %>% mutate(word = str_replace(word, pattern = '#', replacement = ''))
#wordcloud
wordcloud2(tags)
