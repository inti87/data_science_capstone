# Dta prep

# - import clean corpus (full or samle data)
# - build Term-Document Matrix - TDM
# - build term frequency Vector - TFV
# - store TFV as data frame (table)
# - create summary of TFV (word level - counting occurence frequencies)
# - build n-grams data frames 
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
source("./script/func_term_freq_vec_df.R")
source("./script/func_n_gram_df_build.R")
source("./script/func_term_freq_vec_freq_sum.R")
source("./script/func_load_libraries.R")

# Load libraries
libraries <- c("tm", "RWeka", "stringr", "dplyr", "tidyr", "ggplot2", "forcats", "ggwordcloud")
load_lib(libraries)


# Select which type of import: full or sample data
ch.import <- "sample" # "sample" | "full"


# a) news corpus

if(ch.import == "sample"){
  load("./data/data_proc/news_corpus_clean_sample.RData")
}else if(ch.import == "clean.news"){
  load("./data/data_proc/news_corpus_clean_full.RData")
}

## Build Term-Document Matrix
set.seed(11235)
news.TDM <- TermDocumentMatrix(data.en_news.corp.clean)

## Build term frequency vector & term frequency vector stored as data frame
news.TFV <- sapply(data.en_news.corp.clean, termFreq) 
news.TFV.df <- term_freq_vec_df(TFV = news.TFV, 
                                parallel = TRUE, 
                                batch.mode = TRUE, 
                                batch.size = 25000) 

## Term frequency vector word level summarized
news.TFV.df.sum <- term_freq_vec_freq_sum(news.TFV.df)

# Build n-grams (multiple words) - we will also check frequencies of n-grams
set.seed(123)
random_lines <- sample(x = 1:length(data.en_news.corp.clean), size = 10000, replace = F)

# news
news.one.Grams.df   <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 1)
news.two.Grams.df   <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 2)
news.three.Grams.df <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 3)
news.four.Grams.df  <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 4)




# b) twitter corpus


# c) blogs corpus



# Build Term-Document Matrix
set.seed(11235)
news.TDM <- TermDocumentMatrix(data.en_news.corp.clean)
twit.TDM <- TermDocumentMatrix(data.en_twitter.corp.clean)
blog.TDM <- TermDocumentMatrix(data.en_blogs.corp.clean)



## Build term frequency vector & term frequency vector stored as data frame

### TFV vector-list
news.TFV <- sapply(data.en_news.corp.clean, termFreq) 
twit.TFV <- sapply(data.en_twitter.corp.clean, termFreq) 
blog.TFV <- sapply(data.en_blogs.corp.clean, termFreq) 

### TFV vector-data frame
news.TFV.df <- term_freq_vec_df(TFV = news.TFV, parallel = TRUE) 
twit.TFV.df <- term_freq_vec_df(TFV = twit.TFV, parallel = TRUE) 
blog.TFV.df <- term_freq_vec_df(TFV = blog.TFV, parallel = TRUE) 

# save.image("./data/TFV_built.RData")
# load("./data/TFV_built.RData")

## Term frequency vector word level summarized
news.TFV.df.sum <- term_freq_vec_freq_sum(news.TFV.df)
twit.TFV.df.sum <- term_freq_vec_freq_sum(twit.TFV.df)
blog.TFV.df.sum <- term_freq_vec_freq_sum(blog.TFV.df)


# Build n-grams (multiple words) - we will also check frequencies of n-grams
set.seed(123)
random_lines <- sample(x = 1:length(data.en_news.corp.clean), size = 10000, replace = F)

# news
news.one.Grams.df   <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 1)
news.two.Grams.df   <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 2)
news.three.Grams.df <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 3)
news.four.Grams.df  <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 4)

# twit
twit.one.Grams.df   <- n_gram_df_build(corpus = data.en_twitter.corp.clean[random_lines], n = 1)
twit.two.Grams.df   <- n_gram_df_build(corpus = data.en_twitter.corp.clean[random_lines], n = 2)
twit.three.Grams.df <- n_gram_df_build(corpus = data.en_twitter.corp.clean[random_lines], n = 3)
twit.four.Grams.df  <- n_gram_df_build(corpus = data.en_twitter.corp.clean[random_lines], n = 4)

# blog
blog.one.Grams.df   <- n_gram_df_build(corpus = data.en_blogs.corp.clean[random_lines], n = 1)
blog.two.Grams.df   <- n_gram_df_build(corpus = data.en_blogs.corp.clean[random_lines], n = 2)
blog.three.Grams.df <- n_gram_df_build(corpus = data.en_blogs.corp.clean[random_lines], n = 3)
blog.four.Grams.df  <- n_gram_df_build(corpus = data.en_blogs.corp.clean[random_lines], n = 4)

