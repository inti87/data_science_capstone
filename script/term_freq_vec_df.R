term_freq_vec_df <- function(news_TFV = news.TFV, parallel = TRUE){
  
  # Prepare documents for data frame creation
  doc.ids <- names(news_TFV) # get ids of all documents
  doc.word.count <- sapply(news_TFV, length) # count words in each document
  doc.ids <- doc.word.count[doc.word.count > 0] # keep only documents with at least one word
  doc.ids <- names(doc.ids)
  
  
  if(!parallel){ # single core processing
      st <- Sys.time() # execution time
      
      TFV.df <- NULL
      for(doc.id in doc.ids){
        words <- names(news_TFV[[doc.id]]) # extract words from given document
        freq <- news_TFV[[doc.id]] # extract frequency of given document
        # merge to data frame
        TFV.temp.df <- data.frame(doc_id = doc.id,
                                  word = words,
                                  frequency = freq)
        # store to main data frame
        TFV.df <- rbind(TFV.df, TFV.temp.df)
      }
      rownames(TFV.df) <- NULL
      
      # convert freq to number
      TFV.df <- TFV.df %>% 
        mutate(frequency = as.numeric(freq))
      
      et <- Sys.time()
      print(et - st)
      
      return(TFV.df)
      
  }else if(parallel){ # parallel core processing
    
    TFV.temp.df.create <- function(doc.id_){
      words <- names(news_TFV[[doc.id_]]) # extract words from given document
      freq <- news_TFV[[doc.id_]] # extract frequency of given document
      # merge to data frame
      TFV.temp.df <- data.frame(doc_id = doc.id_,
                                word = words,
                                frequency = freq)
      return(TFV.temp.df)
    }
  
  st <- Sys.time()
  
  library(foreach)
  library(doParallel)
  
    #setup parallel backend to use many processors
    cores <- detectCores()
    cl <- makeCluster(cores[1]-1) #not to overload your computer
    registerDoParallel(cl)
    
    finalMatrix <- foreach(i=1:length(doc.ids), .combine = rbind) %dopar% {
      tempMatrix = TFV.temp.df.create(doc.ids[i]) #calling a function
    }
    
    rownames(finalMatrix) <- NULL
    
    # convert freq to number
    finalMatrix <- finalMatrix %>% 
      mutate(frequency = as.numeric(frequency))
    
    #stop cluster
    stopCluster(cl)
    
    et <- Sys.time()
    print(et - st)
    
    return(finalMatrix)
  }
  
}
