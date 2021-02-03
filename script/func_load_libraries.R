#' Install & load packages
#' 
#' @param packages list of packages (stored as strings) to be installed and/or loaded into R session 
#' @return loaded packages
#' @author Marko Intihar


load_lib <- function(packages){
  lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
}

