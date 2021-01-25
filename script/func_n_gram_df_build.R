n_gram_df_build <- function(corpus, n){
  
  # measure eval time
  ts <- Sys.time()
  
  # base n-grams table
  df.n.grams.base <- data.frame(text = sapply(corpus, as.character), 
                                stringsAsFactors = F)
  
  # n-gram token
  n.Gram.Token <- NGramTokenizer(df.n.grams.base, 
                                 Weka_control(min = n, max = n))
  
  # final data frame - word(n-gram) ~ occurence frequency
  n.Grams <- data.frame(table(n.Gram.Token))
  colnames(n.Grams) <- c("Word", "Frequency") 
  
  print(Sys.time() - ts)
  return(n.Grams)
}
