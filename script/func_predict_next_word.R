#' Predict next word from a given phrase
#'
#' This function takes a phrase as an input and tries to predict the next word in a sequence
#' 
#' @param corpus a clean corpus 
#' @param phrase input phrase
#' @param k parameter which determines a minimum frequency for ngram word search in corpus
#' @author Marko Intihar

predict_next_word <- function(phrase, 
                         k = 5){
  
  # clean initial phrase/string
  string <- suppressWarnings(clean_corpus(Corpus(VectorSource(phrase)))$content)
  
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
  
  # top frequent word search - nesting mode
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
    word_predicted <- df.words_predicted %>% 
      mutate(freq_ok = case_when(count_words_prior < 5 ~ FALSE,
                                 TRUE ~ TRUE)) %>% 
      arrange(desc(freq_ok), desc(probability)) %>% 
      head(1) %>% 
      pull(word_predicted)
    return(word_predicted)
  }
}
