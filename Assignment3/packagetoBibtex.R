# set working directory (change this line to your repository location)
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Assignment3")

packs <- c("repmis", "foreign", "dplyr", "stargazer", "knitr", "Zelig", "rms", "markdown")
repmis::LoadandCite(packs, file="Package_Citation.bib")