#How to use this file:
##Highlight all code (for example, on a PC, just hit ctrl+a to select all).
##With all of the code highlighted, click "run" in the top right of Rstudio.
###This will pull reference files from GitHub and launch the program locally.
###Please be patient, it may take a minute or two. 

install.packages(c("shiny", "shinydashboard", "plyr", "clubSandwich","metaSEM",))
install.packages(c("ggrepel","metafor","ggplot2","dplyr","data.table"))
library(shiny); library(shinydashboard); library(plyr); library(grid); library(clubSandwich); library(metaSEM); library(ggrepel); library(metafor)
library(ggplot2); library(dplyr); library(data.table)
runGitHub(repo = "simple_meta-analysis", username = "noah-schroeder", ref="main")
