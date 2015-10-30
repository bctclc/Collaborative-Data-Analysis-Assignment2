### Datasets Cleanup ###
### Claire & Noriko ###

setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/datasets")

library(foreign)
library(dplyr)

# drop variables that are only available in survey 2014
year14 <- read.dta("f1402_mec.dta")
data14 <- subset(year14, 
            select= -c(MEC_9, AGEX2, AGEX3, NatldS1, NatldS2, NatldS3, NatldS4,
            NatldS5, NatldS6, NatldW1, NatldW2, NatldW3, NatldW4, NatldW5, NatldW6))

# drop variables that are only available in survey 2015
year15 <- read.dta("f1502_mec.dta")
data15 <- subset(year15, select= -Intro4)

# make column names the same 
colnames(data14)[which(names(data14) == "highed4")] <- "HighEd4"
colnames(data14)[which(names(data14) == "NatldE1")] <- "NatID1"
colnames(data14)[which(names(data14) == "NatldE2")] <- "NatID2"
colnames(data14)[which(names(data14) == "NatldE3")] <- "NatID3"
colnames(data14)[which(names(data14) == "NatldE4")] <- "NatID4"
colnames(data14)[which(names(data14) == "NatldE5")] <- "NatID5"
colnames(data14)[which(names(data14) == "NatldE6")] <- "NatID6"


# combine the two datasets
data1415 <- rbind(data14, data15)

# drop unwanted variables
finaldata <- subset(data1415, select= c(MEC_6, MEC_7M01, MEC_7M02, MEC_7M03,
            MEC_7M04, MEC_7M05, MEC_7M06, MEC_7M07, MEC_7M08,MEC_7M09, MEC_7M10,
            MEC_8M1, MEC_8M2, MEC_8M3, MEC_10M01, MEC_10M02, MEC_10M03, MEC_10M04,
            MEC_10M05,MEC_10M06, MEC_10M07, MEC_10M08, MEC_10M09, MEC_10M10, 
            MEC_11M1, MEC_11M2, MEC_11M3, RSEX, RAGE, GorA, DVHsize, Respmar, CAR,
            HighEd1, IllLim, DVILO3a, FtPtWk, NSECAC3, sumgross, NumDepCh, MEC_1,
            MEC_3))

# convert multipul choice questions into binary dummy by category
values <- unique(finaldata$MEC_7M01) %>% as.character
values <- values[!(values %in% NA)]
for (i in values) {
  finaldata[, i] <- 0
  finaldata[, i][finaldata$MEC_7M01==i|
                              finaldata$MEC_7M02==i|
                              finaldata$MEC_7M03==i|
                              finaldata$MEC_7M04==i|
                              finaldata$MEC_7M05==i|
                              finaldata$MEC_7M06==i|
                              finaldata$MEC_7M07==i|
                              finaldata$MEC_7M08==i|
                              finaldata$MEC_7M09==i|
                              finaldata$MEC_7M10==i] <- 1
}



# save the dataset
save(finaldata, file="clean_dataset.rda")





