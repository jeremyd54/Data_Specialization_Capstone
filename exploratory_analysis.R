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
#Make data frame
twitBi <- tibble(entry = 1:length(twitLines), text = twitLines) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word)
#Save to disc
save(twitBi, file = "twitBi.RData")
#Count commons
twitBiCom <- twitBi %>% 
    count(word1, word2, sort = TRUE) %>%
    unite(bigram, word1, word2, sep = " ")
#Clear space and look
rm(twitBi)
head(twitBiCom, n = 10)
#Remove non-characters and view
twitBiCom <- twitBiCom[grep("^[0-9a-z ]+$", twitBiCom$bigram), ]
head(twitBiCom, n = 20)
#Plot
twitBiPlot <- twitBiCom %>%
    filter(n > 1314) %>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot(aes(n, bigram)) + ggtitle("Twitter") +
    geom_col(fill = "red")
#Save and remove
save(twitBiCom, file = "twitBiCom.RData")
rm(twitBiCom)

### Blog
#Make data frame and save
blogBi <- tibble(text = blogLines) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word)
save(blogBi, file = "blogBi.RData")
##Count Commons and clear space
blogBiCom <- blogBi %>%
    count(word1, word2, sort = TRUE) %>%
    unite(bigram, word1, word2, sep = " ")
#Clear space and view
rm(blogBi)
head(blogBiCom, n = 10)
##Remove non-characters and view
blogBiCom <- blogBiCom[grep("^[0-9a-z ]+$", blogBiCom$bigram), ]
head(blogBiCom, n = 20)
#Plot
blogBiPlot <- blogBiCom %>%
    filter(n > 840) %>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot(aes(n, bigram)) + ggtitle("Blogs") +
    geom_col(fill = "red")
save(blogBiCom, file = "blogBiCom.RData")
rm(blogBiCom)

## News

#Make data frame and save
newsBi <- tibble(text = newsLines) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word)
save(newsBi, file = "newsBi.RData")
#Count common and clear
newsBiCom <- newsBi %>%
    count(word1, word2, sort = TRUE) %>%
    unite(bigram, word1, word2, sep = " ")
#View and clear space
head(newsBiCom, n = 10)
rm(newsBi)
#Remove non-characters
newsBiCom <- newsBiCom[grep("^[0-9a-z ]+$", newsBiCom$bigram), ]
head(newsBiCom, n = 20)
save(newsBiCom, file = "newsBiCom.RDAta")
#Plot
newsBiPlot <- newsBiCom %>%
    filter(n > 126) %>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot(aes(n, bigram)) + ggtitle("News") +
    geom_col(fill = "red")
rm(newsBiCom)

## Plot together
grid.arrange(twitBiPlot, blogBiPlot, newsBiPlot, nrow = 1,
             top = "Common Bi-Grams")
