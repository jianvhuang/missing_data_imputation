#' Load All Required Packages for ImputationR
#'
#' This function loads all the packages required for the ImputationR workflow.
#' If any package is not installed, it will attempt to install it.
#'
#' @export
#' @return Invisible NULL
#' @examples
#' \dontrun{
#' load_imputation_dependencies()
#' }
load_imputation_dependencies <- function() {
  required_packages <- c("tidyr", "dplyr", "mice", "caret", "DMwR2", "polycor", 
                         "ggplot2", "VIM", "patchwork", "reshape2", "missForest", 
                         "miceRanger", "rmarkdown")
  
  for(pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing package:", pkg))
      install.packages(pkg)
    }
    
    library(pkg, character.only = TRUE)
  }
  
  invisible(NULL)
}