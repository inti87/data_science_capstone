#' Aggregate summarize frequencies of term frequency vector (data frame format) on word level
#'
#' This function converts calculates total frequencies of word occurence in TFV data frame
#' 
#' @param TFV.df a term frequency vector in data frame format
#' @return a term frequency vector in data frame format with frequencies summarized on a word level
#' @author Marko Intihar


term_freq_vec_freq_sum <- function(TFV.df){
  TFV.df.sum <- TFV.df %>% 
    group_by(word) %>% 
    summarise(in_docs = n(), 
              frequency = sum(frequency)) %>% 
    ungroup() %>% 
    arrange(desc(frequency)) %>% 
    mutate(frequency_total_cum_sum = cumsum(frequency),
           frequency_total = sum(frequency),
           rank_word = row_number(),
           percent_words_covered = frequency_total_cum_sum / frequency_total) %>% 
    select(rank_word, everything())
  
  return(TFV.df.sum)
}

