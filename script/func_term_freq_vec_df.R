term_freq_vec_df <- function(TFV, 
                             parallel = TRUE,
                             batch.mode = TRUE,
                             batch.size = 10000){
  
  
  # Batch settings
  
  if(batch.mode){
    # number of batches
    nr.batches <- ceiling(length(TFV) / batch.size)  
    
    # determine start & end line for each batch
    batch.lines <- data.frame(batch = 1:nr.batches,
                              start = seq(1, (nr.batches - 1) * batch.size + 1, batch.size),
                              end   = c(seq(batch.size, (nr.batches - 1) * batch.size, batch.size), length(TFV)))
  }else{ # only one batch
    # number of batches
    nr.batches <- 1
    
    # determine start & end line for each batch
    batch.lines <- data.frame(batch = 1,
                              start = 1,
                              end   = length(TFV))
  }
  
  TFV.batch.df.all <- NULL # here we will store data from each batch
  
  for(i in 1:nr.batches){ # do calculations for each batch
    
    # Prepare data for each batch
    line.start <- batch.lines %>% filter(batch == i) %>% pull(start)
    line.end   <- batch.lines %>% filter(batch == i) %>% pull(end)
    TFV.batch  <- TFV[line.start:line.end]
    
  
    # Prepare documents for data frame creation
    doc.ids <- names(TFV.batch) # get ids of all documents
    doc.word.count <- sapply(TFV.batch, length) # count words in each document
    doc.ids <- doc.word.count[doc.word.count > 0] # keep only documents with at least one word
    doc.ids <- names(doc.ids)
    
    
    if(!parallel){ # single core processing
        st <- Sys.time() # execution time
        
        TFV.batch.df <- NULL
        
        for(doc.id in doc.ids){
          
          words <- names(TFV.batch[[doc.id]]) # extract words from given document
          freq <- TFV.batch[[doc.id]] # extract frequency of given document
          
          # merge to data frame
          TFV.batch.temp.df <- data.frame(doc_id = doc.id,
                                    word = words,
                                    frequency = freq)
          # store to main data frame
          TFV.batch.df <- rbind(TFV.batch.df, TFV.batch.temp.df)
        }
        rownames(TFV.batch.df) <- NULL
        
        # convert freq to number
        TFV.batch.df <- TFV.batch.df %>% 
          mutate(frequency = as.numeric(frequency))
        
               
        # store to main batch data frame
        TFV.batch.df.all <- rbind(TFV.batch.df.all, TFV.batch.df) 
		
		et <- Sys.time()
        print(".......................")
        print(paste("batch", i, sep = " "))
        print(et - st)
        
    }else if(parallel){ # parallel core processing
      
      TFV.batch.temp.df.create <- function(doc.id_){
        words <- names(TFV.batch[[doc.id_]]) # extract words from given document
        freq <- TFV.batch[[doc.id_]] # extract frequency of given document
        # merge to data frame
        TFV.batch.temp.df <- data.frame(doc_id = doc.id_,
                                  word = words,
                                  frequency = freq)
        return(TFV.batch.temp.df)
      }
    
    st <- Sys.time()
    
    library(foreach)
    library(doParallel)
    
      #setup parallel backend to use many processors
      cores <- detectCores()
      cl <- makeCluster(cores[1]-1) #not to overload your computer
      registerDoParallel(cl)
      
      finalMatrix <- foreach(i=1:length(doc.ids), .combine = rbind) %dopar% {
        tempMatrix = TFV.batch.temp.df.create(doc.ids[i]) #calling a function
      }
      
      rownames(finalMatrix) <- NULL
      
      # convert freq to number
      finalMatrix <- finalMatrix %>% 
        mutate(frequency = as.numeric(frequency))
      
      #stop cluster
      stopCluster(cl)
           
      # store to main batch data frame
      TFV.batch.df.all <- rbind(TFV.batch.df.all, finalMatrix) 
	  
	  et <- Sys.time()
      print(".......................")
      print(paste("batch", i, sep = " "))
      print(et - st)
    }
  }
  
  return(TFV.batch.df.all)
}

