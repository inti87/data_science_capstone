import_text <- function(path, enc = "UTF-8"){
  
  file.connection <- file(path)
  
  data.raw <- readLines(con = file.connection, 
                        encoding = enc)
  close(file.connection)
  return(data.raw)
}
