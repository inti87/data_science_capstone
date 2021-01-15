# Exploratory Data Analysis (EDA)

rm(list =ls())
graphics.off()

library(tm)
library(stringr)
library(dplyr)

source("./script/sample_lines.R")
source("./script/import_text.R")
source("./script/corpus_cleaning.R")

# Import clean data
ch.import <- "sample" # sample or full data?

if(ch.import == "sample"){
  
  load("./data/clean_sample_data.RData")
  
}else(load("./data/clean_data.RData"))

# EDA

## Check summaries - size of corpus
length(data.en_news.corp.clean)
length(data.en_blogs.corp.clean)
length(data.en_twitter.corp.clean)


## Build Term-Document Matrix
set.seed(11235)

news.corp.dtm <- TermDocumentMatrix(data.en_news.corp.clean)
news.corp.dtm <- as.matrix(news.corp.dtm) # convert to matrix
