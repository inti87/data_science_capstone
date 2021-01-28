# Import & clean data

# - import raw corpus text data (news, twitter, blogs)
# - create Vcorpus
# - clean corpus
# - sample data
# - save image (full data, sampled data)

# author: Marko Intihar

rm(list =ls())
graphics.off()

# procedure time (start)
st <- Sys.time() 

# Load functions
source("./script/func_sample_lines.R")
source("./script/func_import_text.R")
source("./script/func_corpus_cleaning.R")
source("./script/func_load_libraries.R")

# Load libraries
libraries <- c("tm", "SnowballC", "stringr", "dplyr")
load_lib(libraries)


# Import data

## Corpus data source

list.files("./data/Coursera-SwiftKey/final/en_US/")

# loading a text file from local computer
data.en_news.raw    <- import_text(path = "./data/Coursera-SwiftKey/final/en_US/en_US.news.txt")
data.en_blogs.raw   <- import_text(path = "./data/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
data.en_twitter.raw <- import_text(path = "./data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")

# creates character vectors
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


# Save image (data)

## corpus full (separate by file)
save(data.en_news.corp.clean,    file = "./data/data_proc/news_corpus_clean_full.RData")
save(data.en_twitter.corp.clean, file = "./data/data_proc/twit_corpus_clean_full.RData")
save(data.en_blogs.corp.clean,   file = "./data/data_proc/blog_corpus_clean_full.RData")

## corpus sample (separate by file)

### sample clean data
set.seed(123)
data.en_news.corp.clean    <- sample_lines(text = data.en_news.corp.clean,    type = "number", number = 100000)
data.en_blogs.corp.clean   <- sample_lines(text = data.en_blogs.corp.clean,   type = "number", number = 100000)
data.en_twitter.corp.clean <- sample_lines(text = data.en_twitter.corp.clean, type = "number", number = 100000)

save(data.en_news.corp.clean,    file = "./data/data_proc/news_corpus_clean_sample.RData")
save(data.en_twitter.corp.clean, file = "./data/data_proc/twit_corpus_clean_sample.RData")
save(data.en_blogs.corp.clean,   file = "./data/data_proc/blog_corpus_clean_sample.RData")
