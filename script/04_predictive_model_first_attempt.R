# Predictive Model

# - Build predictive model main goal
# - Create frequency counts and conditional probability for n-gram tokens
# - ...
# - ...

# author: Marko Intihar

rm(list =ls())
graphics.off()


# Load functions
source("./script/func_load_libraries.R")
source("./script/func_conditional_prob_n_grams.R")


# Load libraries
libraries <- c("dplyr", "tidyr", "ggplot2", "stringr", "stringi")
load_lib(libraries)


# Import data (from prep procedure step)
load("./data/data_proc/02_data_prep_news_all_objects.RData")
load("./data/data_proc/02_data_prep_twit_all_objects.RData")
load("./data/data_proc/02_data_prep_blog_all_objects.RData")

 
# Prepare tables for probability estimation
# - bi-gram    split to 2 columns prior               word  ~ predicted word
# - three-gram split to 2 columns prior word1,        word2 ~ predicted word
# - four-gram  split to 2 columns prior word1, word2, word3 ~ predicted word

freq.2.gram.news.probs <- conditional_prob_n_grams(news.bi.Grams.df)
freq.3.gram.news.probs <- conditional_prob_n_grams(news.three.Grams.df)
freq.4.gram.news.probs <- conditional_prob_n_grams(news.four.Grams.df)


# Simple model

# Split string
string   <- "body"
strings  <- strsplit(string, " ") %>% unlist()
words.nr <- length(strings)

if(words.nr == 0){
  message("Please insert at least one word!")
}else if(words.nr == 1){
  n2.token <- strings
  
  prediction.n2 <- freq.2.gram.news.probs %>% 
    filter(words_prior == n2.token) %>% 
    arrange(desc(probability)) %>% 
    head(1) %>% 
    pull(word_predicted)
  print(prediction.n2)
    
}else if(words.nr == 2){
  n2.token <- strings[2]
  n3.token <- strings[(words.nr-1):words.nr] %>% paste(collapse = " ")
  
  prediction.n2 <- freq.2.gram.news.probs %>% 
    filter(words_prior == n2.token) %>% 
    arrange(desc(probability)) %>% 
    head(1) %>% 
    pull(word_predicted)
  print(prediction.n2)
  
  prediction.n3 <- freq.3.gram.news.probs %>% 
    filter(words_prior == n3.token) %>% 
    arrange(desc(probability)) %>% 
    head(1) %>% 
    pull(word_predicted)
  print(prediction.n3)
  
}else if(words.nr >= 3){
  n2.token <- strings[words.nr]
  n3.token <- strings[(words.nr-1):words.nr] %>% paste(collapse = " ")
  n4.token <- strings[(words.nr-2):words.nr] %>% paste(collapse = " ")
  
  
  prediction.n2 <- freq.2.gram.news.probs %>% 
    filter(words_prior == n2.token) %>% 
    arrange(desc(probability)) %>% 
    head(1) %>% 
    pull(word_predicted)
  print(prediction.n2)
  
  prediction.n3 <- freq.3.gram.news.probs %>% 
    filter(words_prior == n3.token) %>% 
    arrange(desc(probability)) %>% 
    head(1) %>% 
    pull(word_predicted)
  print(prediction.n3)
  
  prediction.n4 <- freq.4.gram.news.probs %>% 
    filter(words_prior == n4.token) %>% 
    arrange(desc(probability)) %>% 
    head(1) %>% 
    pull(word_predicted)
  print(prediction.n4)
  
}




