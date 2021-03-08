# Prepare tables for predictive model

# - Build n-gram tables for word prediction
# - Create frequency counts and conditional probability for n-gram tokens

# author: Marko Intihar

rm(list = ls())
graphics.off()


# Load functions
source("./script/func_load_libraries.R")
source("./script/func_conditional_prob_n_grams.R")
source("./script/func_merge_n_grams.R")

# Load libraries
libraries <- c("dplyr", "tidyr", "ggplot2", "stringr", "stringi", "tm")
load_lib(libraries)


# Import data (from prep procedure step)
load("./data/data_proc/02_data_prep_news_all_objects.RData")
load("./data/data_proc/02_data_prep_twit_all_objects.RData")
load("./data/data_proc/02_data_prep_blog_all_objects.RData")

# Merge n-grams data frames (different corpus-es) into one data frame
n.grams.1.df <- merge_n_grams(list(news.uni.Grams.df, twit.uni.Grams.df, blog.uni.Grams.df)) 
n.grams.2.df <- merge_n_grams(list(news.bi.Grams.df, twit.bi.Grams.df, blog.bi.Grams.df)) 
n.grams.3.df <- merge_n_grams(list(news.three.Grams.df, twit.three.Grams.df, blog.three.Grams.df)) 
n.grams.4.df <- merge_n_grams(list(news.four.Grams.df, twit.four.Grams.df, blog.four.Grams.df)) 


# Prepare tables for probability estimation
# - bi-gram    split to 2 columns prior               word  ~ predicted word
# - three-gram split to 2 columns prior word1,        word2 ~ predicted word
# - four-gram  split to 2 columns prior word1, word2, word3 ~ predicted word
freq.2.gram.news.probs <- conditional_prob_n_grams(n.grams.2.df)
freq.3.gram.news.probs <- conditional_prob_n_grams(n.grams.3.df)
freq.4.gram.news.probs <- conditional_prob_n_grams(n.grams.4.df)

# Save relevant data to .RData object - for Shiny app (next word prediction)
save(freq.2.gram.news.probs,
     freq.3.gram.news.probs,
     freq.4.gram.news.probs,
     file = "./data/data_for_model/sample_100k_docs_cleaning_stop_bad_stem.RData")

