# Import & clean data

rm(list =ls())
graphics.off()

# procedure time (start)
st <- Sys.time() 

library(tm)
library(stringr)
library(dplyr)

source("./script/sample_lines.R")
source("./script/import_text.R")
source("./script/corpus_cleaning.R")

# Import data

## Corpus data source

list.files("./data/Coursera-SwiftKey/final/en_US/")

# specifies the exact folder where my text file(s) is for analysis with tm.
# corpus.en_US <- Corpus(DirSource("./data/Coursera-SwiftKey/final/en_US/"), 
#                        readerControl = list(language="en"))
# 
# summary(corpus.en_US)  #check what went in

#loading a text file from local computer
data.en_news.raw    <- import_text(path = "./data/Coursera-SwiftKey/final/en_US/en_US.news.txt")
data.en_blogs.raw   <- import_text(path = "./data/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
data.en_twitter.raw <- import_text(path = "./data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")


#Load data as corpus
#VectorSource() creates character vectors
data.en_news.corp.raw    <- VCorpus(VectorSource(data.en_news.raw))
data.en_blogs.corp.raw   <- VCorpus(VectorSource(data.en_blogs.raw))
data.en_twitter.corp.raw <- VCorpus(VectorSource(data.en_twitter.raw))

## Other sources

### Profanity - bad words
profanity_words <- read.csv(file = "http://www.bannedwordlist.com/lists/swearWords.txt") %>% 
  pull() %>% 
  VectorSource(.)
  

# Clean data (corpus data)
data.en_news.corp.clean    <- clean_corpus(corpus = data.en_news.corp.raw)
data.en_blogs.corp.clean   <- clean_corpus(corpus = data.en_blogs.corp.raw)
data.en_twitter.corp.clean <- clean_corpus(corpus = data.en_twitter.corp.raw)


# procedure end time
et <- Sys.time() 
exec.time <- et - st; print(exec.time)

# Save clean data
save.image(file = "./data/clean_data.RData")


# Sample data & save sampled data
data.en_news.corp.clean    <- sample_lines(text = data.en_news.corp.clean, type = "percentage", percentage = .05)
data.en_blogs.corp.clean   <- sample_lines(text = data.en_blogs.corp.clean, type = "percentage", percentage = .05)
data.en_twitter.corp.clean <- sample_lines(text = data.en_twitter.corp.clean, type = "percentage", percentage = .05)

save(data.en_news.corp.clean, 
     data.en_blogs.corp.clean, 
     data.en_twitter.corp.clean, 
     file = "./data/clean_sample_data.RData")

