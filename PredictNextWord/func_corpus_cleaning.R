#' Clean provided corpus
#'
#' This function cleans corpus (text) by:
#' - removing punctuation
#' - converting text to lower case
#' - removing stop words
#' - stripping white spaces
#' - removing profanity words (list provided by user)
#' - removing numbers
#' - steming text
#' 
#' @param corpus a raw corpus text file imported from the disk 
#' @param profanity a list of profanity words (provided by user)
#' @return a clean corpus
#' @author Marko Intihar


clean_corpus <- function(corpus, profanity = profanity_words){
  
  corpus_clean <- tm_map(corpus, removePunctuation)                       # remove punctuation
  corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))      # to lower case
  corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english")) # remove stop words
  corpus_clean <- tm_map(corpus_clean, stripWhitespace)                   # remove extra white space
  #corpus_clean <- tm_map(corpus_clean, removeWords, profanity_words)     # remove bad words
  corpus_clean <- tm_map(corpus_clean, removeNumbers)                     # remove numbers words
  #corpus_clean <- tm_map(corpus_clean, stemDocument)                     # stemmed corpus
    
  return(corpus_clean)
}
