library(tm)
library(dplyr)
library(tidyr)
library(tidytext)

## Download data
dataUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("data.zip")){
    download.file(dataUrl, destfile = "data.zip")
    unzip("data.zip")
}
##Read into lines
twitLines <- readLines("./final/en_US/en_US.twitter.txt")
blogLines <- readLines("./final/en_US/en_US.blogs.txt")
newsLines <- readLines("./final/en_US/en_US.news.txt")
##Create Corpus
corpus <- c(twitLines, blogLines, newsLines)
rm(twitLines,blogLines,newsLines)
save(corpus, file = "corpus.RData")
##Sample the data to reduce size to under 1 million lines
set.seed(1007)
smallCorpus <- sample(corpus, size = length(corpus) * .25)
rm(corpus)
gc()
