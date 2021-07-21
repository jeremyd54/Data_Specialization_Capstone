library(tm)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(stringr)

##Create a full Corpus to model from later

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






