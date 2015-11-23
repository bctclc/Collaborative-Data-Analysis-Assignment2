### R codes for more logit models (obstacles to buying an EV) ###

# set working directory
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Final_Paper")

# load the clearned-up dataset
load("EVdata1.rda")

# packages
library(rms)
library(stargazer)


### Main Obstacles ###
obscat <- c('choice', 'knowledge', 'cost', 'battery', 'recharge', 
            'resale', 'safety', 'spec', 'technology')
obsmean <- c(mean(EVINTEREST$POchoice), mean(EVINTEREST$POknowledge),
             mean(EVINTEREST$POcost), mean(EVINTEREST$PObattery),
             mean(EVINTEREST$POrecharge), mean(EVINTEREST$POresale),
             mean(EVINTEREST$POsafety), mean(EVINTEREST$POcarspec),
             mean(EVINTEREST$POtech))
barplot(obsmean, names.arg=obscat, main="Obstacles to buying an EV",
        ylab="% of respondents who choose the category")


# Recharge
Obs1 <- lrm(POrecharge ~ RAGE + Male + lowermiddle + highermiddle +high + degree 
            + licence + NumCar + Scotland,
            EVINTEREST)
lrm(Obs1)

# Battery
Obs2 <- lrm(PObattery ~ RAGE + Male + lowermiddle + highermiddle +high + degree 
            + licence + NumCar + Scotland,
            EVINTEREST)
lrm(Obs2)

# Cost
Obs3 <- lrm(POcost ~ RAGE + Male + lowermiddle + highermiddle +high + degree 
            + licence + NumCar + Scotland,
            EVINTEREST)
lrm(Obs3)

# summary table
stargazer(Obs1, Obs2, Obs3, type="text", header=F, 
          digits = 2, title="Determinats of obstacles")


### Breakdown of concerns re costs ###
cobscat <- c('purchase', 'fuel', 'maintenance', 'resale', 'exercise tax',
             'company tax', 'insurance')
cobsmean <- c(mean(EVINTEREST$POCpurchase), mean(EVINTEREST$POCfuel),
              mean(EVINTEREST$POCmaintenance), mean(EVINTEREST$POCresale),
              mean(EVINTEREST$POCextax), mean(EVINTEREST$POCcomptax),
              mean(EVINTEREST$POCinsurance))
barplot(cobsmean, names.arg=cobscat, main="Obstacles to buying an EV regarding costs",
        ylab="% of respondents who choose the category")


# Purchase
Cobs1 <- lrm(POCpurchase ~ RAGE + Male + lowermiddle + highermiddle +high + degree 
             + licence + NumCar + Scotland,
             EVINTEREST)
lrm(Cobs1)

# maintenance
Cobs2 <- lrm(POCmaintenance ~ RAGE + Male + lowermiddle + highermiddle +high + degree 
             + licence + NumCar + Scotland,
             EVINTEREST)
lrm(Cobs2)

# Cost
Cobs3 <- lrm(POCfuel ~ RAGE + Male + lowermiddle + highermiddle +high + degree 
             + licence + NumCar + Scotland,
             EVINTEREST)
lrm(Cobs3)

# summary table
stargazer(Cobs1, Cobs2, Cobs3, type="text", header=F, 
          digits = 2, title="Determinats of obstacles regarding costs")
