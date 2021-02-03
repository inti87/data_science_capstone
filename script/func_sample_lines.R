#' Sample lines of corpus text for creating text samples
#'
#' This function creates samples of text from provided text corpus
#' 
#' @param text a text corpus(raw, clean or some other text stored in lines)
#' @param type parameter: "number" - sample lines based on number (of lines to be sampled) | "percentage" - sample % of total lines
#' @param number parameter determining how many lines should be sampled (if you use type = "number") 
#' @param percentage parameter determining % of lines should be sampled (if you use type = "percentage") 
#' @return a sampled text corpus
#' @author Marko Intihar


sample_lines <- function(text, type = "number", number = 1000, percentage = 0.1){
  
  nr_lines <- length(text) # total number of lines
  
  if(type == "number"){ # sample selected number of lines
    
    lines.sampled.id <- sample(x = 1:nr_lines, size = number, replace = F)
    
  } else if(type == "percentage"){ # sample percentage of total lines
    
    lines.sampled.id <- sample(x = 1:nr_lines, size = round(nr_lines * percentage), replace = F)
    
  } else(stop("Choose number or percentage (for type)"))
  
  lines.sampled <- text[lines.sampled.id]
  
  return(lines.sampled)
}