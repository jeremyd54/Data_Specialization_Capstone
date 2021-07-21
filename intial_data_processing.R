library(tm)
library(dplyr)
library(tidyr)
library(tidytext)

## Download and unzip data
dataUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("data.zip")){
    download.file(dataUrl, destfile = "data.zip")
    unzip("data.zip")
}

#Read in data and transform into tall format

twitLines <- readLines("./final/en_US/en_US.twitter.txt")
twit <- tibble(entry = 1:length(twitLines), text = twitLines) %>%
    unnest_tokens(word, text)

blogLines <- readLines("./final/en_US/en_US.blogs.txt")
blog <- tibble(entry = 1:length(blogLines), text = blogLines) %>%
    unnest_tokens(word, text)

newsLines <- readLines("./final/en_US/en_US.news.txt")
news <- tibble(entry = 1:length(newsLines), text = newsLines) %>%
    unnest_tokens(word, text)





