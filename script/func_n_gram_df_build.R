#' Create n-gram tokens data frame
#'
#' This function creates frequency - occurence of of n-gram words in probvided corpus
#' 
#' @param corpus a clean corpus 
#' @param n parameter for determining n-gram size
#' @param batch.mode option for creating table in smaller batches : using smaller batches (TRUE) and not using batches all in one run (FALSE)
#' @param batch.size number determining batch size for splitting clean corpus
#' @return a n-gram frequency data frame
#' @return execution time (for each batch)
#' @author Marko Intihar


n_gram_df_build <- function(corpus, 
                            n,
                            batch.mode = TRUE,
                            batch.size = 10000){
  
  # Batch settings
  
  if(batch.mode){
    # number of batches
    nr.batches <- ceiling(length(corpus) / batch.size)  
    
    # determine start & end line for each batch
    batch.lines <- data.frame(batch = 1:nr.batches,
                              start = seq(1, (nr.batches - 1) * batch.size + 1, batch.size),
                              end   = c(seq(batch.size, (nr.batches - 1) * batch.size, batch.size), length(corpus)))
  }else{ # only one batch
    # number of batches
    nr.batches <- 1
    
    # determine start & end line for each batch
    batch.lines <- data.frame(batch = 1,
                              start = 1,
                              end   = length(corpus))
  }
  
  n.Grams.all <- NULL # here we will store data from each batch
  
  for(i in 1:nr.batches){ # do calculations for each batch
    
    # measure eval time
    ts <- Sys.time()
    
    # Prepare data for each batch
    line.start <- batch.lines %>% filter(batch == i) %>% pull(start)
    line.end   <- batch.lines %>% filter(batch == i) %>% pull(end)
    corpus.batch  <- corpus[line.start:line.end]
    
    
    # base n-grams table
    df.n.grams.base.batch <- data.frame(text = sapply(corpus.batch, as.character), 
                                        stringsAsFactors = F)
    
    # n-gram token
    n.Gram.Token.batch <- NGramTokenizer(df.n.grams.base.batch, 
                                         Weka_control(min = n, max = n))
    
    # final data frame - word(n-gram) ~ occurence frequency
    n.Grams.batch <- data.frame(table(n.Gram.Token.batch))
    colnames(n.Grams.batch) <- c("Word", "Frequency") 
    
       
    # store to main batch data frame
    n.Grams.all <- rbind(n.Grams.all, n.Grams.batch)

	print(".......................")
    print(paste("batch", i, sep = " "))
    print(Sys.time() - ts)
    
  }


# Aggregate n-gram word frequencies for all batches
# - sum all occurrence (frequencies) 
n.Grams.final <- n.Grams.all %>% 
  group_by(Word) %>% 
  summarise(Frequency = sum(Frequency)) %>% 
  ungroup()

return(n.Grams.final)
}
