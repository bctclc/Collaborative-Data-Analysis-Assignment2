### Datasets Cleanup ###
### Claire & Noriko ###

setwd("E:\—ßœ∞œ‡πÿ\R\Collaborative-Data-Analysis-Assignment2\datasets")

library(foreign)

# drop variables that are only available in survey 2014
year14 <- read.dta("f1402_mec.dta")
data14 <- subset(year14, 
            select= -c(MEC_9, AGEX2, AGEX3, NatldS1, NatldS2, NatldS3, NatldS4,
            NatldS5, NatldS6, NatldW1, NatldW2, NatldW3, NatldW4, NatldW5, NatldW6))

# drop variables that are only available in survey 2015
year15 <- read.dta("f1502_mec.dta")
data15 <- subset(year15, select= -Intro4)

# combine the two datasets
data1415 <- rbind(data14, data15)
