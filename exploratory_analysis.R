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





