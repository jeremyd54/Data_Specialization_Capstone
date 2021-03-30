source("intial_data_processing.R")
library(ggplot2)
library(gtable)
library(gridExtra)

## Import stop words
data("stop_words")

### Find common words

## twitter
twitCommon <- twit %>%
    count(word, sort = TRUE)
head(twitCommon, n = 20)
#Remove stop words
twitCommonStop <- twit %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
head(twitCommonStop, n = 20)

## Blogs
blogCommon <- blog %>%
    count(word, sort = TRUE)
head(blogCommon, n = 20)
#Remove stop words
blogCommonStop <- blog %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
head(blogCommonStop, n = 20)

## News
newsCommon <- news %>%
    count(word, sort = TRUE)
head(newsCommon, n = 20)
#Remove stop words
newsCommonStop <- news %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
head(newsCommonStop, n = 20)

## Plot common words
#Twitter
twitComP1 <- twitCommon %>% 
    filter(n > 168000) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) + ggtitle("With Stop Words") +
    geom_col(fill = "blue")   
twitComP2 <- twitCommonStop %>%
    filter(n > 28450) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) + ggtitle("Stop Words Removed") +
    geom_col(fill = "red")
grid.arrange(twitComP1,twitComP2, nrow = 1, 
             top = "Most Common Twitter Words")
#Blogs
blogComP1 <- blogCommon %>%
    filter(n > 218000) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) + ggtitle("With Stop Words") + 
    geom_col(fill = "blue")
blogComP2 <- blogCommonStop %>%
    filter(n > 21400) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) + ggtitle("Stop Words Removed") + 
    geom_col(fill = "red")
grid.arrange(blogComP1, blogComP2, nrow = 1, 
             top = "Most Common Blog Words")
#News
newsComP1 <- newsCommon %>%
    filter(n > 11600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) + ggtitle("With Stop Words") +
    geom_col(fill = "blue")
newsComP2 <- newsCommonStop %>%
    filter(n > 1797) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) + ggtitle("Stop Words Removed") +
    geom_col(fill = "red")
grid.arrange(newsComP1, newsComP2, nrow = 1,
             top = "Most Common News Words")

## Clear out RAM space
save(blog, news, twit, file = "sources.RData")
save(blogCommon, newsCommon, twitCommon, file = "common.RData")
save(blogCommonStop, newsCommonStop, twitCommonStop, file = "commonNoStop.RData")
rm(blogCommonStop, newsCommonStop, twitCommonStop,
   blogCommon, newsCommon, twitCommon, blog, news, twit)

### Bi-Grams

## Twitter
twitBi <- tibble(entry = 1:length(twitLines), text = twitLines) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word)
twitBiCom <- twitBi %>% 
    count(word1, word2, sort = TRUE) %>%
    unite(bigram, word1, word2, sep = " ")
twitBiPlot <- twitBiCom %>%
    filter


blogBi <- tibble(entry = 1:length(blogLines), text = blogLines) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) 

newsBi <- tibble(entry = 1:length(newsLines), text = newsLines) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
