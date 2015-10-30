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
# MEC_7 (waht would put you off...)
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

# combining same categories
#############################
# the labels from 2014 and 2015 are unfortunately slightly different (horrible job of UK Data Service!)
# I combine the following pairs together
#  "Technology: doesn t work/not proven" 
#  "Technology: doesn t work / not proven"
#
#  "Safety features/record" 
#  "Safety features / record"
#
#  "The vehicle: performance, size/practicality, looks" 
#  "The vehicle: performance (e.g. speed / handling), size / practicality, looks"
#
#  "Other (please specify)" 
#  "Other (please specify)/"
#
#  "Resale/residual value"
#  "Value:Resale/residual"
#############################
finaldata$`Technology: doesn t work/not proven` <- finaldata$`Technology: doesn t work/not proven`+
                                                   finaldata$`Technology: doesn t work / not proven`
finaldata$`Safety features/record` <- finaldata$`Safety features/record`+finaldata$`Safety features / record`
finaldata$`The vehicle: performance, size/practicality, looks` <- finaldata$`The vehicle: performance, size/practicality, looks`+
                                                                  finaldata$`The vehicle: performance (e.g. speed / handling), size / practicality, looks`
finaldata$`Other (please specify)` <- finaldata$`Other (please specify)`+finaldata$`Other (please specify)/`
finaldata$`Resale/residual value` <- finaldata$`Resale/residual value`+finaldata$`Value:Resale/residual`

# rename the variables
colnames(finaldata)[which(names(finaldata) == "Battery: distance travelled on charge")] <- "PObattery"
colnames(finaldata)[which(names(finaldata) == "The vehicle: performance, size/practicality, looks")] <- "POchrct"
colnames(finaldata)[which(names(finaldata) == "Other (please specify)")] <- "POother"
colnames(finaldata)[which(names(finaldata) == "Lack of knowledge")] <- "POlknowledge"
colnames(finaldata)[which(names(finaldata) == "Recharging")] <- "POrecharge"
colnames(finaldata)[which(names(finaldata) == "Technology: doesn t work/not proven")] <- "POtech"
colnames(finaldata)[which(names(finaldata) == "Limited choice (not many vehicles to choose from)")] <- "POchoice"
colnames(finaldata)[which(names(finaldata) == "Cost")] <- "POcost"
colnames(finaldata)[which(names(finaldata) == "Resale/residual value")] <- "POresale"
colnames(finaldata)[which(names(finaldata) == "Safety features/record")] <- "POsafety"

# drop unecessary dummies created by the loop
#############################
# I wanted to drop those variables that automatically created by the loop, but that we won't use
# (such as "refusal", "don't know", and the one of the duplicated variables I combinded above).
# However, since they contain spaces & wired charaters like "," and "(", 
# the command you did above (select -c(columnames)) didn't work. 
# I ended up renaming all those variables to drop.
############################
colnames(finaldata)[which(names(finaldata) == "Nothing")] <- "drop1"
colnames(finaldata)[which(names(finaldata) == "Don t know")] <- "drop2"
colnames(finaldata)[which(names(finaldata) == "Refusal")] <- "drop3"
colnames(finaldata)[which(names(finaldata) == "Don t know (Spontaneous only)")] <- "drop4"
colnames(finaldata)[which(names(finaldata) == "Technology: doesn t work / not proven")] <- "drop5"
colnames(finaldata)[which(names(finaldata) == "Other (please specify)/")] <- "drop6"
colnames(finaldata)[which(names(finaldata) == "Safety features / record")] <- "drop7"
colnames(finaldata)[which(names(finaldata) == "The vehicle: performance (e.g. speed / handling), size / practicality, looks")] <- "drop8"
colnames(finaldata)[which(names(finaldata) == "Refusal (Spontaneous only)")] <- "drop9"
colnames(finaldata)[which(names(finaldata) == "Value:Resale/residual")] <- "drop10"
finaldata <- subset(finaldata, 
               select= -c(drop1, drop2, drop3, drop4, drop5, drop6, drop7, drop8, drop9, drop10))

                          
# continue this for other multipulchoice questions!


# save the dataset
save(finaldata, file="clean_dataset.rda")
