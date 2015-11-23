### R codes for more logit models ###

# set working directory
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Final_Paper")

# load the clearned-up dataset
load("EVdata1.rda")

# packages
library(rms)
library(stargazer)

# Main Obstacles
obscat <- c('choice', 'knowledge', 'cost', 'battery', 'recharge', 
            'resale', 'safety', 'carspec', 'tech')
obsmean <- c(mean(EVINTEREST$POchoice), mean(EVINTEREST$POknowledge),
             mean(EVINTEREST$POcost), mean(EVINTEREST$PObattery),
             mean(EVINTEREST$POrecharge), mean(EVINTEREST$POresale),
             mean(EVINTEREST$POsafety), mean(EVINTEREST$POcarspec),
             mean(EVINTEREST$POtech))
barplot(obsmean, names.arg=obscat, main="Obstacles to buying an EV")


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
