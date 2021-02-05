#' Aggregate summarize frequencies of term frequency vector (data frame format) on word level
#'
#' This function merges n-gram data frames (multiple corpus-es)
#' 
#' @param df.list a list of n-gram data frames from different corpus sources
#' @return a merged (and word level summarized) n-gram data frame
#' @author Marko Intihar
#' 

merge_n_grams <- function(df.list){
  
  n.grams.df <- NULL # empty data frame
  
  # merge n-gram data frames
  for(i in 1:length(df.list)){
    n.grams.df <- bind_rows(n.grams.df, df.list[[i]])  
  }
  
  # summarize frequencies on word level
  n.grams.df <- n.grams.df %>% 
    group_by(Word) %>% 
    summarise(Frequency = sum(Frequency)) %>% 
    ungroup()
  
  return(n.grams.df)
}

  