# Import data

rm(list =ls())
graphics.off()

library(tm)
library(stringr)
library(dplyr)

source("./script/sample_lines.R")

# Import data

list.files("./data/Coursera-SwiftKey/final/en_US/")

# specifies the exact folder where my text file(s) is for analysis with tm.
# corpus.en_US <- Corpus(DirSource("./data/Coursera-SwiftKey/final/en_US/"), 
#                        readerControl = list(language="en"))
# 
# summary(corpus.en_US)  #check what went in

#loading a text file from local computer
data.en_news    <- readLines("./data/Coursera-SwiftKey/final/en_US/en_US.news.txt", encoding = "UTF-8")
data.en_blogs   <- readLines("./data/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", encoding = "UTF-8")
data.en_twitter <- readLines("./data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", encoding = "UTF-8")


#Load data as corpus
#VectorSource() creates character vectors
data.en_news.corp <- Corpus(VectorSource(data.en_news))
data.en_blogs.corp <- Corpus(VectorSource(data.en_blogs))
data.en_twitter.corp <- Corpus(VectorSource(data.en_twitter))


# Clean data

# convert to lower case
data.en_news.corp <- tm_map(data.en_news.corp, content_transformer(tolower))

#remove ������ what would be emojis
data.en_news.corp <- tm_map(data.en_news.corp, content_transformer(gsub), pattern="\\W",replace=" ")

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeURL)
)
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeNumPunct))
# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))
#u can create custom stop words using the code below.
#myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
#mydata <- tm_map(mydata, removeWords, myStopwords)
# remove extra whitespace
mydata <- tm_map(mydata, stripWhitespace)
# Remove numbers
mydata <- tm_map(mydata, removeNumbers)
# Remove punctuations
mydata <- tm_map(mydata, removePunctuation)

