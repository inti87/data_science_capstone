doc.ids <- names(news.TFV) # get ids of all documents
doc.word.count <- sapply(news.TFV, length) # count words in each document
doc.ids <- doc.word.count[doc.word.count > 0] # keep only documents with at least one word
doc.ids <- names(doc.ids)
#doc.ids <- doc.ids[1:20000]



st <- Sys.time()
TFV.df <- NULL
for(doc.id in doc.ids){
  words <- names(news.TFV[[doc.id]]) # extract words from given document
  freq <- news.TFV[[doc.id]] # extract frequency of given document
  
  # merge to data frame
  TFV.temp.df <- data.frame(doc_id = doc.id,
                            word = words,
                            frequency = freq)
  
  # store to main data frame
  TFV.df <- rbind(TFV.df, TFV.temp.df)
}
et <- Sys.time()
et - st


library(foreach)
library(doParallel)

TFV.temp.df.create <- function(doc.id){
  words <- names(news.TFV[[doc.id]]) # extract words from given document
  freq <- news.TFV[[doc.id]] # extract frequency of given document
  
  # merge to data frame
  TFV.temp.df <- data.frame(doc_id = doc.id,
                            word = words,
                            frequency = freq)
  return(TFV.temp.df)
  
}

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

st <- Sys.time()
finalMatrix <- foreach(i=1:length(doc.ids), .combine = rbind) %dopar% {
  tempMatrix = TFV.temp.df.create(doc.ids[i]) #calling a function
  #do other things if you want
  
  tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
}

rownames(finalMatrix) <- NULL

# convert freq to number
finalMatrix <- finalMatrix %>% 
  mutate(freq = as.numeric(freq))

#stop cluster
stopCluster(cl)

et <- Sys.time()
et - st




unlist(news.TFV[["75604"]])
words <- names(news.TFV[["75604"]])
freq <- news.TFV[["75604"]]

cbind(words, freq)
