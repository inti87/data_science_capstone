#' Import raw corpus text file from disk to R session
#' 
#' @param path a path to corpus text file 
#' @param enc parameter for determining corpus text encoding
#' @return an imported raw corpus
#' @author Marko Intihar


import_text <- function(path, enc = "UTF-8"){
  
  file.connection <- file(path)
  
  data.raw <- readLines(con = file.connection, 
                        encoding = enc)
  close(file.connection)
  return(data.raw)
}
