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
    set.seed(1008)
    halfCorpus <- sample(corpus, size = length(corpus) * .4) ##Use ~half corpus
    cleanCorpus <- halfCorpus %>%               
        sapply(replace_non_ascii) %>%           ##Remove non-ascii characters
        tibble(text = .) %>%                    ##Reformat data
        mutate(text = str_replace_all(text, "[0-9]", "")) %>% ##Remove numbers
        mutate(text = str_replace_all(text, "_", ""))  ##Remove underscores
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
## Make a corpus without stop words
stopCorpus <- uniStop %>%  
    group_by(entry) %>%                         ##Group the entries
    summarize(text = str_c(word, collapse = " ")) %>%  ##Recombine
    ungroup() %>% 
    select(-entry)                              ##Remove entry column
save(stopCorpus, file = "stopCorpus.rda")       ##Save for later
## Format uni-grams for later
uni <- uni[ , 2]                                ##Remove entry column
names(uni) <- "response"                        ##Change column name
uniStop <- uniStop[ , 2]
names(uniStop) <- "response"
save(uni, uniStop, file = "unis.rda")           ##Save for later
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
    separate(bigram, c("words", "response"), sep = " ")          ##Separate words
  biStop <- unnest_tokens(trainStop, bigram, text, "ngrams", n = 2) %>% ##Create bi-grams
    separate(bigram, c("words", "response"), sep = " ")          ##Separate words
  save(bi, biStop, file = "bis.rda")            ##Save for later
  rm(bi,biStop)                                 ##Clear space
}

##Tri-grams
if(!file.exists("tris.rda")) {
  tri <- unnest_tokens(train, trigram, text, "ngrams", n = 3) %>%  ##Create tri-grams
    separate(trigram, c("word1", "word2", "response"), sep = " ") %>% ##separate words
    unite(words, c("word1", "word2"), sep = "_")                   ##Recombine first 2 words
  triStop <- unnest_tokens(trainStop, trigram, text, "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "response"), sep = " ") %>%
    unite(words, c("word1", "word2"), sep = "_")
  save(tri, triStop, file = "tris.rda")        ##Save for later
  rm(tri, triStop)                             ##Clear space  
}

##Quad-grams
if(!file.exists("quads.rda")) {
  quad <- unnest_tokens(train, trigram, text, "ngrams", n = 4) %>% ##Create quad-grams
    separate(trigram, c("word1", "word2", "word3", "response"), sep = " ") %>% ##separate words
    unite(words, c("word1", "word2", "word3"), sep = "_")                  ##Recombine first 3 words
  quadStop <- unnest_tokens(trainStop, trigram, text, "ngrams", n = 4) %>%
    separate(trigram, c("word1", "word2", "word3", "response"), sep = " ") %>%
    unite(words, c("word1", "word2", "word3"), sep = "_") 
  save(quad, quadStop, file = "quads.rda")
} else {load("quads.rda")}
rm(train, trainStop)                           ##Clear space


## Count the frequency of n-grams and only ones which appear more than 3 times
## to help reduce memory usage
countFreq <- function(tbl) { 
  if(ncol(tbl) == 1) {                   ##For Uni-grams
    freq <- count(tbl, response)         ##Count the frequencies   
  }
  else {                                 ##For all other n-grams
    freq <-count(tbl, response, words)   ##Count frequencies by lead-up words
  }
  freq <- freq[freq$n > 2, ]             ##Only use counts greater than 2
  return(freq)
}
## Load data sets back into memory and count frequencies 
if(!file.exists("counts.rda")) {
  load("unis.rda")  
  load("bis.rda")
  load("tris.rda")
  uniCount <- countFreq(uni)
  uniCountStop <- countFreq(uniStop)
  biCount <- countFreq(bi)
  biCountStop <- countFreq(biStop)
  triCount <- countFreq(tri)
  triCountStop <- countFreq(triStop)
  quadCount <- countFreq(quad)
  quadCountStop <- countFreq(quadStop)
  ##Save for later and clear space
  save(uniCount, biCount, triCount, quadCount, file = "counts.rda")
  save(uniCountStop, biCountStop, triCountStop, quadCountStop, 
       file = "countsStop.rda")
  rm(uniCountStop, biCountStop, triCountStop, quadCountStop,     
     uni, bi, tri, quad, uniStop, biStop, triStop, quadStop)
} else {load("counts.rda")}

##


















