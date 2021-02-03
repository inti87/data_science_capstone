#' Function for calculating table of conditional probabilities for n-gram words 
#'
#' This function:
#' - takes data frame of n-gram frequency occurence
#' - splits each n-gram word into word predicted (last word) - w_i and words prior to predicted word (word sequence before last word) - w_1 w_2 ... w_i-1
#' - calculates conditional probability using frequencies 
#' - p(w_i|w_i-1 ... w_i-n+1) = c(w_i-1 ... w_i-n+1 w_i) / c(w_i-1 ... w_i-n+1
#' 
#' @details
#' This is baseline for our n-gram language model for predicting the 
#' next most obvious word by providing a word sequence!
#'  
#' @param n_gram_df a clean corpus 
#' @return a n-gram frequency data frame
#' @author Marko Intihar


conditional_prob_n_grams <- function(n_gram_df){
  
  # additonal removing non-alphanumeric characters 
  n_gram_df <- n_gram_df %>% 
    mutate(strange_chars = str_detect(string = Word, pattern = "[^a-zA-Z0-9 ]")) %>%  # match anything but letters, numbers and white space (space at the end)
    filter(!strange_chars) # remove non-alphanumberic characters
  
  # prepare table for word sequence frequencies counts
  freq.n.gram <- n_gram_df %>% 
    # rename fields
    rename(word_sequence = Word,
           count_word_sequence = Frequency) 
  
  # define word split position
  split_position <- stri_locate_last(str = freq.n.gram %>% pull(word_sequence), charclass = "\\p{Zs}") %>% .[,1] # position of split
  number_chars   <- str_length(freq.n.gram %>% pull(word_sequence)) # number of characters before split
  
  # define vectors for splitting words
  words_prior_start <- rep(1, nrow(freq.n.gram))  # prior words start sequence
  words_prior_end   <- split_position - 1         # prior words end sequence
  word_predicted_start <- split_position + 1      # predicted word start position
  word_predicted_end   <- number_chars            # predicted word end position
  
  # split words into prior words, predicted word
  freq.n.gram <- freq.n.gram %>% 
    mutate(words_prior = str_sub(word_sequence, start = words_prior_start, end = words_prior_end),
           word_predicted = str_sub(word_sequence, start = word_predicted_start, end = word_predicted_end)) 
  
  
  # calculate conditional probability using frequencies
  #          p(w_i|w_i-1 ... w_i-n+1) = c(w_i-1 ... w_i-n+1 w_i) / c(w_i-1 ... w_i-n+1
  # example bi-gram: "last year"  p(year|last) = c(last year) / c(last)
  freq.n.gram.probs <- freq.n.gram %>% 
    # add counts for c(ww_i-1 ... w_i-n+1) - words prior
    group_by(words_prior) %>% 
    mutate(count_words_prior = sum(count_word_sequence)) %>% 
    ungroup() %>% 
    # calculate probabilities
    mutate(probability = count_word_sequence / count_words_prior)
  
  return(freq.n.gram.probs)
}
