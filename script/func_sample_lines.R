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