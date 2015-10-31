### Datasets Cleanup ver2 (inport data w/o variable labels) ###
### Claire & Noriko ###

setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/datasets")

library(foreign)
library(dplyr)

# drop variables that are only available in survey 2014
year14 <- read.dta("f1402_mec.dta", convert.factors = FALSE)
data14 <- subset(year14, 
            select= -c(MEC_9, AGEX2, AGEX3, NatldS1, NatldS2, NatldS3, NatldS4,
            NatldS5, NatldS6, NatldW1, NatldW2, NatldW3, NatldW4, NatldW5, NatldW6))

# drop variables that are only available in survey 2015
year15 <- read.dta("f1502_mec.dta", convert.factors = FALSE)
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
# MEC_7 (waht would put you off... (PO))
POcategoly <- c(1:9)
for (i in POcategoly){
  finaldata[, paste("PO", as.character(i), sep = "")] <- 0
  finaldata[, paste("PO", as.character(i), sep = "")][finaldata$MEC_7M01==i|
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

# MEC_8 (3 options that would put you off... (cost)) (POC)
POCcategoly <- c(1:7)
for (i in POCcategoly){
  finaldata[, paste("POC", as.character(i), sep = "")] <- 0
  finaldata[, paste("POC", as.character(i), sep = "")][finaldata$MEC_8M1==i|
                                                       finaldata$MEC_8M2==i|
                                                       finaldata$MEC_8M3==i] <- 1
}

# MEC_10 (what would encourage you...) (EN)
ENcategory <- c(1:9)
for (i in ENcategory){
  finaldata[, paste("EN", as.character(i), sep = "")] <- 0
  finaldata[, paste("EN", as.character(i), sep = "")][finaldata$MEC_10M01==i|
                                                      finaldata$MEC_10M02==i|
                                                      finaldata$MEC_10M03==i|
                                                      finaldata$MEC_10M04==i|
                                                      finaldata$MEC_10M05==i|
                                                      finaldata$MEC_10M06==i|
                                                      finaldata$MEC_10M07==i|
                                                      finaldata$MEC_10M08==i|
                                                      finaldata$MEC_10M09==i|
                                                      finaldata$MEC_10M10==i] <- 1
}

# MEC_11 (3 options that would encounrage... (cost)) (ENC)
ENCcategory <- c(1:7)
for (i in ENCcategory){
  finaldata[, paste("ENC", as.character(i), sep = "")] <- 0
  finaldata[, paste("ENC", as.character(i), sep = "")][finaldata$MEC_11M1==i|
                                                       finaldata$MEC_11M2==i|
                                                       finaldata$MEC_11M3==i] <- 1
}

#########
# we can rename these variables if we want...
# actually we may have to do so, as everything's required to be human readable...
#########

# categorical income to continuous
## mean value of each category, but 52000 for the largest one (52000+)
INCcategorymean <- c(260, 780, 1300, 1820, 2340, 2860, 3380, 3900, 4420, 4940, 5720,
                     6760, 7800, 8840, 9880, 10920, 11960, 13000, 14040, 15080, 16120,
                     17160, 18200, 19240, 20280, 22100, 24700, 27300, 29900, 32500,
                     35100, 37700, 40300, 42900, 45500, 48100, 50700, 52000)
for (i in 1:38){
  finaldata$income[finaldata$sumgross==i] <- INCcategorymean[i]
}

# Dependent variable into binary dummy (in case we run logit instead of mlogit)
finaldata$EVinterest[finaldata$MEC_6<=4] <- 1
finaldata$EVinterest[finaldata$MEC_6>4 & finaldata$MEC_6<=8] <- 0


# save the dataset
save(finaldata, file="clean_dataset_ver2.rda")
