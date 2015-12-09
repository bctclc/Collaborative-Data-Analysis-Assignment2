# set working directory (change this line to your repository location)
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Final_Paper")

library(repmis)

packs <- c("repmis", "foreign", "dplyr", "stargazer", "knitr", "Zelig", "rms", 
           "markdown", "captioner", "ggplot2", "googleVis", "plyr", "maptools",
           "ggmap", "ggplot2", "RColorBrewer")
repmis::LoadandCite(packs, file="Package_Citation.bib")