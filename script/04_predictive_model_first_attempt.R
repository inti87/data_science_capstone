# Predictive Model

# - Build predictive model main goal
# - Create frequency counts and conditional probability for n-gram tokens
# - ...
# - ...

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


### Profanity - bad words
profanity_words <- read.csv(file = "http://www.bannedwordlist.com/lists/swearWords.txt") %>% 
  pull() %>% 
  VectorSource(.)


#save.image(file = "./data/data_proc/04_data.RData")
#load("./data/data_proc/04_data.RData")


# Simple model

# Split string
string   <- "school"
string <- suppressWarnings(clean_corpus(Corpus(VectorSource(string)))$content)

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




# Split string
string <- "sometim next week"
# string <- "last year although"
string <- "ice cream"
string <- "ice"
string <- "slovenijajaja slovenijajaja slovenijajaja"


# clean initial string
string <- suppressWarnings(clean_corpus(Corpus(VectorSource(string)))$content)

# split string into words
strings  <- strsplit(string, " ") %>% unlist()

# keep only last 4 words or 3,2,1
if(length(strings) >= 4){
  words.nr <- length(strings)
  strings <- strings[(words.nr-2):words.nr]
}

words.nr <- length(strings) # number of prior words


# initial search setting 
#  -- number of steps - depth
#  -- ngram tables


# determine initial n-gram for search
rm(steps, ngrams)
if(words.nr == 1){
  ngrams <- list(s1 = freq.2.gram.news.probs)
  steps <- 1
}else if(words.nr == 2){
  ngrams <- list(s1 = freq.3.gram.news.probs,
                 s2 = freq.2.gram.news.probs)
  steps <- 2
}else{
  ngrams <- list(s1 = freq.4.gram.news.probs,
                 s2 = freq.3.gram.news.probs,
                 s3 = freq.2.gram.news.probs)
  steps <- 3
}

# store results
df.words_predicted <- NULL

# top frequent word search
for(step in 1:steps){
  
  # words prior (for frequency search)
  words_prior_ <- strings[(1+step-1):words.nr] %>% 
    paste(., collapse = " ")
  
  # find most frequent word
  df.word_predicted <- ngrams[[step]] %>% 
    filter(words_prior == words_prior_) %>% 
    arrange(desc(probability)) %>% 
    head(1) 
  
  # store predicted words
  df.words_predicted <- bind_rows(df.words_predicted, df.word_predicted)
  
  # if frequency is less than selected default values continue with search
  # and if nothing is found
  # otherwise stop searching
  if(nrow(df.words_predicted) == 0){ # no word found
    stop_search <- FALSE  
  }else if(df.word_predicted %>% pull(count_words_prior) < 5){ # not so frequent word found
    stop_search <- FALSE  
  }else{
    stop_search <- TRUE
  }
   
  # stopping procedure
  if(stop_search){break}
}

# Final predicted word selection
if(nrow(df.words_predicted) == 0){ # if no word found
  message("Sorry no matching found, please insert a different phrase!")
}else{ # matching found
  df.words_predicted %>% 
    mutate(freq_ok = case_when(count_words_prior < 5 ~ FALSE,
                               TRUE ~ TRUE)) %>% 
    arrange(desc(freq_ok), desc(probability)) %>% 
    head(1) %>% 
    pull(word_predicted)
}



# using a function
load("./data/data_for_model/sample_100k_docs_cleaning_stop_bad_stem.RData")
source("./script/func_corpus_cleaning.R")
source("./script/func_predict_next_word.R")
predict_next_word("dead or")
