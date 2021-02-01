# Data prep

# - import clean corpus (full or samle data)
# - build Term-Document Matrix - TDM
# - build term frequency Vector - TFV
# - store TFV as data frame (table)
# - create summary of TFV (word level - counting occurence frequencies)
# - build n-grams data frames 
# - save image (full data, sampled data)

# author: Marko Intihar

rm(list =ls())
gc()
graphics.off()

# rJava - increase memory size allocated to rJava library
options(java.parameters = "-Xmx10240m") # 10GB allocated
library(rJava)


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


#-----------------------#
#    a) news corpus     #
#-----------------------#

# Select which type of import: full or sample data
ch.import <- "full" # "sample" | "full"

if(ch.import == "sample"){
  load("./data/data_proc/news_corpus_clean_sample.RData")
}else if(ch.import == "full"){
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
news.uni.Grams.df   <- n_gram_df_build(corpus = data.en_news.corp.clean, n = 1, batch.mode = TRUE, batch.size = 10000)
news.bi.Grams.df    <- n_gram_df_build(corpus = data.en_news.corp.clean, n = 2, batch.mode = TRUE, batch.size = 10000)
news.three.Grams.df <- n_gram_df_build(corpus = data.en_news.corp.clean, n = 3, batch.mode = TRUE, batch.size = 10000)
news.four.Grams.df  <- n_gram_df_build(corpus = data.en_news.corp.clean, n = 4, batch.mode = TRUE, batch.size = 10000)

# Save data
save(news.TFV.df.sum, news.uni.Grams.df, 
     news.bi.Grams.df, news.three.Grams.df, 
     news.four.Grams.df,    
     file = "./data/data_proc/02_data_prep_news_only_selected_objects.RData")

save.image(file = "./data/data_proc/02_data_prep_news_all_objects.RData")

# rm(list =ls())
# gc()


#-----------------------#
#  b) twitter corpus    #
#-----------------------#


if(ch.import == "sample"){
  load("./data/data_proc/twit_corpus_clean_sample.RData")
}else if(ch.import == "full"){
  load("./data/data_proc/twit_corpus_clean_full.RData")
}

## Build Term-Document Matrix
set.seed(11235)
twit.TDM <- TermDocumentMatrix(data.en_twitter.corp.clean)

## Build term frequency vector & term frequency vector stored as data frame
twit.TFV <- sapply(data.en_twitter.corp.clean, termFreq) 
twit.TFV.df <- term_freq_vec_df(TFV = twit.TFV, 
                                parallel = TRUE, 
                                batch.mode = TRUE, 
                                batch.size = 25000) 

## Term frequency vector word level summarized
twit.TFV.df.sum <- term_freq_vec_freq_sum(twit.TFV.df)

# Build n-grams (multiple words) - we will also check frequencies of n-grams
set.seed(123)
twit.uni.Grams.df   <- n_gram_df_build(corpus = data.en_twitter.corp.clean, n = 1, batch.mode = TRUE, batch.size = 10000)
twit.bi.Grams.df    <- n_gram_df_build(corpus = data.en_twitter.corp.clean, n = 2, batch.mode = TRUE, batch.size = 10000)
twit.three.Grams.df <- n_gram_df_build(corpus = data.en_twitter.corp.clean, n = 3, batch.mode = TRUE, batch.size = 10000)
twit.four.Grams.df  <- n_gram_df_build(corpus = data.en_twitter.corp.clean, n = 4, batch.mode = TRUE, batch.size = 10000)

# Save data
save(twit.TFV.df.sum, twit.uni.Grams.df, 
     twit.bi.Grams.df, twit.three.Grams.df, 
     twit.four.Grams.df,    
     file = "./data/data_proc/02_data_prep_twit_only_selected_objects.RData")

save.image(file = "./data/data_proc/02_data_prep_twit_all_objects.RData")

# rm(list =ls())
# gc()


#-----------------------#
#    c) blogs corpus    #
#-----------------------#

# Select which type of import: full or sample data
ch.import <- "full" # "sample" | "full"

if(ch.import == "sample"){
  load("./data/data_proc/blog_corpus_clean_sample.RData")
}else if(ch.import == "full"){
  load("./data/data_proc/blog_corpus_clean_full.RData")
}

## Build Term-Document Matrix
set.seed(11235)
blog.TDM <- TermDocumentMatrix(data.en_blogs.corp.clean)

## Build term frequency vector & term frequency vector stored as data frame
blog.TFV <- sapply(data.en_blogs.corp.clean, termFreq) 
blog.TFV.df <- term_freq_vec_df(TFV = blog.TFV, 
                                parallel = TRUE, 
                                batch.mode = TRUE, 
                                batch.size = 25000) 

## Term frequency vector word level summarized
blog.TFV.df.sum <- term_freq_vec_freq_sum(blog.TFV.df)

# Build n-grams (multiple words) - we will also check frequencies of n-grams
set.seed(123)
blog.uni.Grams.df   <- n_gram_df_build(corpus = data.en_blogs.corp.clean, n = 1, batch.mode = TRUE, batch.size = 10000)
blog.bi.Grams.df    <- n_gram_df_build(corpus = data.en_blogs.corp.clean, n = 2, batch.mode = TRUE, batch.size = 10000)
blog.three.Grams.df <- n_gram_df_build(corpus = data.en_blogs.corp.clean, n = 3, batch.mode = TRUE, batch.size = 10000)
blog.four.Grams.df  <- n_gram_df_build(corpus = data.en_blogs.corp.clean, n = 4, batch.mode = TRUE, batch.size = 10000)

# Save data
save(blog.TFV.df.sum, blog.uni.Grams.df, 
     blog.bi.Grams.df, blog.three.Grams.df, 
     blog.four.Grams.df,    
     file = "./data/data_proc/02_data_prep_blog_only_selected_objects.RData")

save.image(file = "./data/data_proc/02_data_prep_blog_all_objects.RData")

rm(list =ls())
gc()

