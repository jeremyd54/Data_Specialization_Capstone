library(tm)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)

## Download data
dataUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("data.zip")){
    download.file(dataUrl, destfile = "data.zip")
    unzip("data.zip")
}
##Reading data into memory
if(!file.exists("corpus.RData")){ ## Check to see if corpus already exists
##Read into lines
twitLines <- readLines("./final/en_US/en_US.twitter.txt")
blogLines <- readLines("./final/en_US/en_US.blogs.txt")
newsLines <- readLines("./final/en_US/en_US.news.txt")
##Create Corpus
corpus <- c(twitLines, blogLines, newsLines)
rm(twitLines,blogLines,newsLines) ##clear memory
save(corpus, file = "corpus.RData") ## Save corpus for later
} else{load("corpus.RData")} ## Load if file already exists

## Sample the data to reduce size to under 1 million lines
if(!file.exists("sampleCorpus.RData")){ ## Check for file
set.seed(1007)
## Sample 25% of the corpus 
smallCorpus <- data.frame(text = sample(corpus, size = length(corpus) * .25)) %>%
    sapply(replace_non_ascii) %>% ## Replace non ascii characters
    tibble(text = .)
save(smallCorpus, file = "sampleCorpus.RData") ##Save sample corpus
} else{load("sampleCorpus.RData")} ## Load if file already exists
rm(corpus) ## Clear memory
gc()

##Create tri-grams and remove stop words
if(!file.exists("triGrams.RData")){ ## Check for file
data("stop_words") #Stop words data set
triGrams <- unnest_tokens(smallCorpus, tri, text, token = "ngrams", n = 3) %>% ## Create tri-grams
    separate(tri, c("word1", "word2", "response"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word, 
           !response %in% stop_words$word) ## Filter out stop words
save(triGrams, file = "triGrams.RData") ## Svae file for later
} else {load("triGrams.RData")} ## Load if file already exists
rm(smallCorpus) ##clear memory
gc()
