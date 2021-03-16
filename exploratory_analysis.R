source("intial_data_processing.R")

## Import stop words
data("stop_words")

### Find common words

## twitter
twitCommon <- twit %>%
    count(word, sort = TRUE)
head(twitCommon, n = 100)
#Remove stop words
twitCommonStop <- twit %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
head(twitCommonStop, n = 10)

## Blogs
blogCommon <- blog %>%
    count(word, sort = TRUE)
head(blogCommon, n = 10)
#Remove stop words
blogCommonStop <- blog %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
head(blogCommonStop, n = 20)

## News
newsCommon <- news %>%
    count(word, sort = TRUE)
head(newsCommon, n = 10)
#Remove stop words
newsCommonStop <- news %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
head(newsCommonStop, n = 10)




