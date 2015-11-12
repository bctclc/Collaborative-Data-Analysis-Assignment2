### Datasets Cleanup (inport data w/o variable labels) ###
### Claire & Noriko ###

### set working directory
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Assignment3")

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


# convert multipul choice questions into binary dummy by category
# MEC_7 (waht would put you off... (PO))
POcategoryname <- c('POchoice', 'POknowledge', 'POcost', 'PObattery', 'POrecharge', 
                    'POresale', 'POsafety', 'POcarspec', 'POtech')
for (i in 1:9){
  data1415[, POcategoryname[i]] <- 0
  data1415[, POcategoryname[i]][data1415$MEC_7M01==i|
                                 data1415$MEC_7M02==i|
                                 data1415$MEC_7M03==i|
                                 data1415$MEC_7M04==i|
                                 data1415$MEC_7M05==i|
                                 data1415$MEC_7M06==i|
                                 data1415$MEC_7M07==i|
                                 data1415$MEC_7M08==i|
                                 data1415$MEC_7M09==i|
                                 data1415$MEC_7M10==i] <- 1
}

# MEC_8 (3 options that would put you off... (cost)) (POC)
POCcategoryname <- c('POCpurchase', 'POCfuel', 'POCmaintenance', 'POCresale', 
                     'POCextax', 'POCcomptax', 'POCinsurance')
for (i in 1:7){
  data1415[, POCcategoryname[i]] <- 0
  data1415[, POCcategoryname[i]][data1415$MEC_8M1==i|
                                  data1415$MEC_8M2==i|
                                  data1415$MEC_8M3==i] <- 1
}

# MEC_10 (what would encourage you...) (EN)
ENcategoryname <- c('ENcost', 'ENbattery', 'ENrecharge', 'ENresale', 'ENsafety', 
                    'ENcarspec', 'ENchoice', 'ENtech', 'ENenv')
for (i in 1:9){
  data1415[, ENcategoryname[i]] <- 0
  data1415[, ENcategoryname[i]][data1415$MEC_10M01==i|
                                 data1415$MEC_10M02==i|
                                 data1415$MEC_10M03==i|
                                 data1415$MEC_10M04==i|
                                 data1415$MEC_10M05==i|
                                 data1415$MEC_10M06==i|
                                 data1415$MEC_10M07==i|
                                 data1415$MEC_10M08==i|
                                 data1415$MEC_10M09==i|
                                 data1415$MEC_10M10==i] <- 1
}

# MEC_11 (3 options that would encounrage... (cost)) (ENC)
ENCcategoryname <- c('ENCpurchase', 'ENCfuel', 'ENCmaintenance', 'ENCresale', 'ENCextax', 
                     'ENCcomptac', 'ENCinsurance')
for (i in 1:7){
  data1415[, ENCcategoryname[i]] <- 0
  data1415[, ENCcategoryname[i]][data1415$MEC_11M1==i|
                                  data1415$MEC_11M2==i|
                                  data1415$MEC_11M3==i] <- 1
}


### variables for our logit models ###
# Dependent 1: interest in EV
data1415$EVinterest[data1415$MEC_6<=4] <- 1
data1415$EVinterest[data1415$MEC_6>4 & data1415$MEC_6<=8] <- 0
table(data1415$EVinterest, data1415$MEC_6)

# SEX dummy
data1415$Male <- 0
data1415$Male[data1415$RSEX==1] <- 1
table(data1415$RSEX, data1415$Male) 

# categorical income to continuous
# mean value of each category, but 52000 for the largest one (52000+)
INCcategorymean <- c(260, 780, 1300, 1820, 2340, 2860, 3380, 3900, 4420, 4940, 5720,
                     6760, 7800, 8840, 9880, 10920, 11960, 13000, 14040, 15080, 16120,
                     17160, 18200, 19240, 20280, 22100, 24700, 27300, 29900, 32500,
                     35100, 37700, 40300, 42900, 45500, 48100, 50700, 52000)
for (i in 1:38){
  data1415$income[data1415$sumgross==i] <- INCcategorymean[i]
}
# Income in GBP1000
data1415$inc1000 <- data1415$income/1000

# categorical income (dummy)
data1415$low <- 0
data1415$low[data1415$sumgross>=1 & data1415$sumgross<20] <- 1
table(data1415$sumgross, data1415$low)
data1415$lowermiddle <- 0
data1415$lowermiddle[data1415$sumgross>=20 & data1415$sumgross<26] <- 1
data1415$highermiddle <- 0
data1415$highermiddle[data1415$sumgross==26 | data1415$sumgross==27] <- 1
data1415$high <- 0
data1415$high[data1415$sumgross>27 & data1415$sumgross<=38] <- 1

### categorical variable for income
data1415$inccat <- 0
data1415$inccat[data1415$low==1] <- 1
data1415$inccat[data1415$lowermiddle==1] <- 2
data1415$inccat[data1415$highermiddle==1] <- 3
data1415$inccat[data1415$high==1] <- 4

# having college degree or not
data1415$degree[data1415$HighEd1==1] <- 1
data1415$degree[data1415$HighEd1>=2 & data1415$HighEd1<=8] <- 0
table(data1415$degree, data1415$HighEd1)

# having a valid driver's licence
data1415$licence[data1415$MEC_1<=2] <- 1
data1415$licence[data1415$MEC_1==3] <- 0
table(data1415$licence, data1415$MEC_1)

# travel everyday?
data1415$travfreq[data1415$MEC_3==1] <- 1
data1415$travfreq[data1415$MEC_3>=2 & data1415$MEC_3<=8] <- 0
table(data1415$travfreq, data1415$MEC_3)

# employment dummy
EmpCategory <- c('Employed', 'Unemployed', 'Inactive')
for (i in 1:3){
  data1415[, EmpCategory[i]] <- 0
  data1415[, EmpCategory[i]][data1415$DVILO3a==i] <- 1
}
table(data1415$Employed, data1415$DVILO3a)
table(data1415$Unemployed, data1415$DVILO3a)
table(data1415$Inactive, data1415$DVILO3a)

# Work status (full-time or part-time)
data1415$Fulltime[data1415$FtPtWk==1] <- 1
data1415$Fulltime[data1415$FtPtWk==2] <- 0
data1415$Parttime[data1415$FtPtWk==2] <- 1
data1415$Parttime[data1415$FtPtWk==1] <- 0
table(data1415$Fulltime, data1415$FtPtWk)
table(data1415$Parttime, data1415$FtPtWk)

# Gov. Region dummy
RegionCategory <- c('NorthEast', 'NorthWest', 'YorkshireHumber', 'EastMidlands', 
                    'WestMidrands', 'EastofEngland', 'London', 'SouthEast', 
                    'SouthWest', 'Wales', 'Scotland')
for (i in 1:11){
  data1415[, RegionCategory[i]] <- 0
  data1415[, RegionCategory[i]][data1415$GorA==i] <- 1
}
table(data1415$NorthEast, data1415$GorA)
table(data1415$NorthWest, data1415$GorA)
table(data1415$YorkshireHumber, data1415$GorA)
table(data1415$EastMidlands, data1415$GorA)
table(data1415$WestMidrands, data1415$GorA)
table(data1415$EastofEngland, data1415$GorA)
table(data1415$London, data1415$GorA)
table(data1415$SouthEast, data1415$GorA)
table(data1415$SouthWest, data1415$GorA)
table(data1415$Wales, data1415$GorA)
table(data1415$Scotland, data1415$GorA)

# marital status
MaritalCategory <- c('Single', 'Married', 'MarriedSep', 'Divorced', 
                     'Widowed')
for (i in 1:5){
  data1415[, MaritalCategory[i]] <- 0
  data1415[, MaritalCategory[i]][data1415$Respmar==i] <- 1
}
table(data1415$Single, data1415$Respmar)
table(data1415$Married, data1415$Respmar)
table(data1415$MarriedSep, data1415$Respmar)
table(data1415$Divorced, data1415$Respmar)
table(data1415$Widowed, data1415$Respmar)

# having Illness/disability
data1415$illness[data1415$LSIll==1] <- 1
data1415$illness[data1415$LSIll==2] <- 0
table(data1415$illness, data1415$LSIll)

# Limited by illness (only for those with illness/disability)
data1415$illnesslim[data1415$IllLim==1] <- 1
data1415$illnesslim[data1415$IllLim==2] <- 0
table(data1415$illnesslim, data1415$IllLim)

# number of cars
data1415$NumCar[data1415$CAR==1] <- 0
data1415$NumCar[data1415$CAR==2] <- 1
data1415$NumCar[data1415$CAR==3] <- 2
data1415$NumCar[data1415$CAR==4] <- 3
table(data1415$NumCar, data1415$CAR)

###dummy for having children or not
data1415$havechildren <-1
data1415$havechildren[data1415$NumDepCh==0] <- 0

### subset of variables for our analysis (w/o missing data)
# base subset
EVINTEREST <- subset(data1415, select= c(EVinterest, RAGE, Male, inc1000, degree, 
                     licence, travfreq, Employed, Unemployed, Inactive, NorthEast, 
                     NorthWest, YorkshireHumber, EastMidlands, WestMidrands, 
                     EastofEngland, London, SouthEast, SouthWest, Wales, Scotland, 
                     Single, Married, MarriedSep, Divorced, Widowed, illness,
                     DVHsize, NumDepCh, NumCar, POchoice, POknowledge, POcost, 
                     PObattery, POrecharge, POresale, POsafety, POcarspec, POtech, 
                     POCpurchase, POCfuel, POCmaintenance, POCresale, POCextax, 
                     POCcomptax, POCinsurance, ENcost, ENbattery, ENrecharge, 
                     ENresale, ENsafety, ENcarspec, ENchoice, ENtech, ENenv, 
                     ENCpurchase, ENCfuel, ENCmaintenance, ENCresale, ENCextax, 
                     ENCcomptac, ENCinsurance, high, highermiddle, lowermiddle, 
                     low, inccat, havechildren))
EVINTEREST <- na.omit(EVINTEREST)

# base subset + full-time/part-time (fewer observations!)
EVINTERESTemp <- subset(data1415, select= c(EVinterest, RAGE, Male, inc1000, degree, 
                        licence, travfreq,  Employed, Unemployed, Inactive, 
                        NorthEast, NorthWest, YorkshireHumber, EastMidlands, 
                        WestMidrands, EastofEngland, London, SouthEast, 
                        SouthWest, Wales, Scotland, Single, Married, MarriedSep, 
                        Divorced, Widowed, illness, DVHsize, NumDepCh, NumCar, 
                        Fulltime, Parttime, POchoice, POknowledge, POcost, 
                        PObattery, POrecharge, POresale, POsafety, POcarspec, 
                        POtech, POCpurchase, POCfuel, POCmaintenance, POCresale, 
                        POCextax, POCcomptax, POCinsurance, ENcost, ENbattery, 
                        ENrecharge, ENresale, ENsafety, ENcarspec, ENchoice, 
                        ENtech, ENenv, ENCpurchase, ENCfuel, ENCmaintenance, 
                        ENCresale, ENCextax, ENCcomptac, ENCinsurance, high, 
                        highermiddle, lowermiddle, low, inccat, havechildren))
EVINTERESTemp <- na.omit(EVINTERESTemp)

# subset of people w/ illness (breakdown of them by limited activity)
EVINTERESTill <- subset(data1415, select= c(EVinterest, RAGE, Male, inc1000, degree, 
                        licence, travfreq, Employed, Unemployed, Inactive, NorthEast, 
                        NorthWest, YorkshireHumber, EastMidlands, WestMidrands, 
                        EastofEngland, London, SouthEast, SouthWest, Wales, Scotland, 
                        Single, Married, MarriedSep, Divorced, Widowed, DVHsize, 
                        NumDepCh, NumCar, illnesslim, POchoice, POknowledge, POcost, 
                        PObattery, POrecharge, POresale, POsafety, POcarspec, POtech, 
                        POCpurchase, POCfuel, POCmaintenance, POCresale, POCextax, 
                        POCcomptax, POCinsurance, ENcost, ENbattery, ENrecharge, 
                        ENresale, ENsafety, ENcarspec, ENchoice, ENtech, ENenv, 
                        ENCpurchase, ENCfuel, ENCmaintenance, ENCresale, ENCextax, 
                        ENCcomptac, ENCinsurance, high, highermiddle, lowermiddle, 
                        low, inccat, havechildren))
EVINTERESTill <- na.omit(EVINTERESTill)

# save the subsets
save(EVINTEREST, file="EVdata1.rda")


