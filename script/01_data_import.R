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
data.en_news <- readLines("./data/Coursera-SwiftKey/final/en_US/en_US.news.txt", encoding = "UTF-8")

#Load data as corpus
#VectorSource() creates character vectors
data.en_news.corp <- Corpus(VectorSource(data.en_news))


# Sample tex (lines sampling)
set.seed(12345)

data.en_news.sample <- sample_lines(text = data.en_news, type = "number", number = 1000)




