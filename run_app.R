runApp <- function() {
  # List of required packages
  packages <- c("shiny", "shinydashboard", "plyr", "grid", "clubSandwich", "metaSEM", "ggrepel", "metafor", "ggplot2", "dplyr", "data.table")
  
  # Function to install and load packages
  install_and_load <- function(packages) {
    for (pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
      } else {
        library(pkg, character.only = TRUE)
      }
    }
  }
  
  # Install and load required packages
  install_and_load(packages)
  
  # Run the Shiny app from GitHub
  shiny::runGitHub(repo = "simple_meta-analysis", username = "noah-schroeder", ref = "main")
}
