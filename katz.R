library(tm)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(stringr)

##Katz model with and without stop-words

##Data cleaning
if(!file.exists("cleanCorpus.rda")) {           ##Check if file is saved
    load("corpus.RData")                        ##From modeling.R
    halfCorpus <- sample(corpus, size = length(corpus) * .4) ##Use ~half corpus
    cleanCorpus <- halfCorpus %>%               
        sapply(replace_non_ascii) %>%           ##Remove non-ascii characters
        tibble(text = .) %>%                    ##Reformat data
        mutate(text = str_replace_all(text, "[0-9]", "")) ##Remove numbers
    save(cleanCorpus, file = "cleanCorpus.rda") ##Save for later
    rm(halfCorpus, corpus)                      ##Clear space
} else {load("cleanCorpus.rda")}                ##Load saved file

##Uni-grams
if(!file.exists("stopCorpus.rda")) {            ##Check if file already exists
    uni <- tibble(entry = 1:nrow(cleanCorpus), text = cleanCorpus$text) %>% ##Add entry row
        unnest_tokens(word, text)               ##Separate into single words
    ##Remove stop words
    data("stop_words")                          ##Stop-words data set
    snowball <- stop_words[stop_words$lexicon == "snowball", 1] ##snowball sub-set
    uniStop <- anti_join(uni, snowball)         ##Remove stop words
    save(uni, uniStop, file = "unis.rda")
## Make a corpus without stop words
stopCorpus <- uniStop %>%  
    group_by(entry) %>%                         ##Group the entries
    summarize(text = str_c(word, collapse = " ")) %>%  ##Recombine
    ungroup() %>% 
    select(-entry)                              ##Remove entry column
save(stopCorpus, file = "stopCorpus.rda")       ##Save for later
rm(uni, uniStop)                                ##Clear space
} else {load("stopCorpus.rda")}

##Separate in training and test sets
set.seed(1008)
inTrain <- sort(sample(nrow(cleanCorpus), nrow(cleanCorpus) * .75)) ##Use 75% for training
train <- cleanCorpus[inTrain, ]                 ##Training set
test <- cleanCorpus[-inTrain, ]                 ##Test set
##Stop words
set.seed(1008)
inTrainStop <- sort(sample(nrow(stopCorpus), nrow(stopCorpus) * .75)) ##Use 75% for training
trainStop <- stopCorpus[inTrainStop, ]          ##Training set
testStop <- stopCorpus[-inTrainStop, ]          ##Test set
save(test, testStop, file = "tests.rda")        ##Save for later
rm(cleanCorpus, stopCorpus, test, testStop,    ##Clear space
   inTrain, inTrainStop)

##Bi-grams
if(!file.exists("bis.rda")) {
  bi <- unnest_tokens(train, bigram, text, "ngrams", n = 2) %>%  ##Create bi-grams
    separate(bigram, c("word1", "response"), sep = " ")          ##Separate words
  biStop <- unnest_tokens(trainStop, bigram, text, "ngrams", n = 2) %>% ##Create bi-grams
    separate(bigram, c("word1", "response"), sep = " ")          ##Separate words
  save(bi, biStop, file = "bis.rda")            ##Save for later
  rm(bi,biStop)                                 ##Clear space
}

##Tri-grams
if(!file.exists("tris.rda")) {
  tri <- unnest_tokens(train, trigram, text, "ngrams", n = 3) %>%  ##Create tri-grams
    separate(trigram, c("word1", "word2", "response"), sep = " ") ##separate words
  triStop <- unnest_tokens(trainStop, trigram, text, "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "response"), sep = " ")
  save(tri, triStop, file = "tris.rda")        ##Save for later
  rm(tri, triStop)                             ##Clear space  
}

##Quad-grams
if(!file.exists("quads.rda")) {
  quad <- unnest_tokens(train, trigram, text, "ngrams", n = 4) %>% ##Create quad-grams
    separate(trigram, c("word1", "word2", "word3", "response"), sep = " ") ##separate words
  quadStop <- unnest_tokens(trainStop, trigram, text, "ngrams", n = 4) %>%
    separate(trigram, c("word1", "word2", "word3", "response"), sep = " ")
  save(quad, quadStop, file = "quads.rda")
}



  
  
