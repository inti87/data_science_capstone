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