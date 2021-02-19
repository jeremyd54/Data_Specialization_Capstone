library(tm)

## Download and unzip data
dataUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(dataUrl, destfile = "data.zip")
unzip("data.zip")


## Make text collections
eng <- Corpus(DirSource("./final/en_US"),
              readerControl = list(reader = readPlain, language = "en_US",
                                   load = TRUE))
            
