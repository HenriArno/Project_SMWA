##########-----Word Embeddings-----##########

#setwd-------------------------------------------------------------------------
rm(list=ls())
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")
#load packages-----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(text2vec,Rtsne, scales,ggrepel,tidyverse,tm)

#import dataset----------------------------------------------------------------
#data <- read_csv("dataset_cleaned.csv") werkt niet om zo te tokenizen?????????
data <- read.csv("./sources/cleaned/dataset_cleaned.csv", 
                    header=TRUE, 
                    stringsAsFactors=FALSE)
#data <- sample_n(data, 1000) 
text<- data %>% select(text)

#tokenize and create vocab-----------------------------------------------------
tokens <- space_tokenizer(text[,1] %>%
                            removePunctuation() %>% 
                            removeWords(words = stopwords()))

it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

#Remove sparse words (=appear less than 3????? times)----------------------------
vocab <- prune_vocabulary(vocab, term_count_min = 500)   #prune 5 of 3?

vectorizer <- vocab_vectorizer(vocab) #one-hot vector met enkel een 1 voor dat woord. output is vector met zelfde aantal componenten die, voor elk woord in de vocab, de kans geeft dat een willekeurig geselecteerd woord naast dit woord staat?

#Use skip gram window of 5 for context words to create co-occurrence matrix-------- predicting surrounding words given a current word
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

#Fit the model------------------------------------------------------------------
glove <- GloVe$new(rank = 100, x_max = 100)   
glove$n_dump_every = 10

word_vectors_main <- glove$fit_transform(tcm, n_iter = 100) #misschien meer dan 20

#obtain word vector-------------------------------------------------------------
word_vectors_components <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_components) #transpose hier krijg je de volledige hidden layer table/ word vector lookup table. hoe groot moet de rank zijn?

##############################################################################################
show <- word_vectors["show", ,drop = FALSE] 
cos_sim <- sim2(x = word_vectors, y = show, 
                method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)
##############################################################################################


#Plot the word embeddings using tsne--------------------------------------------
keep_words <- setdiff(rownames(word_vectors), stopwords())
word_vec <- word_vectors[keep_words,]

#prepare data frame to train tsne for visualization-----------------------------
train_df <- data.frame(word_vec) %>%
  rownames_to_column("word")
tsne <- Rtsne(train_df[,-1], 
              dims = 2,perplexity = 50,
              verbose=TRUE, max_iter = 500)

# create plot-------------------------------------------------------------------
colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)

plot_df <- data.frame(tsne$Y) %>%
  mutate(
    word = train_df$word,
    col = colors[train_df$word]
  ) %>%
  left_join(vocab, by = c("word" = "term")) %>%
  filter(doc_count >= 10)

p <- ggplot(plot_df, aes(X1, X2)) +
  geom_text(aes(X1, X2, label = word, color = col), size = 4) +
  xlab("") + ylab("") +
  theme(legend.position = "none") 
p



