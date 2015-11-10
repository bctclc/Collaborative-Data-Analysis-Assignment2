### logit models for assignment 3 ###
### Claire & Noriko ###

library(stargazer)
library(knitr)
library(Zelig)
library(rms)

# set working directory (change this line to your repository location)
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/datasets")

# road the cleaned up dataframe
load("EVdata1.rda")
load("EVdata2.rda")
load("EVdata3.rda")


### step-wise logistic regression ###
# Estimate model-1 (AGE & SEX & INCOME & Education & LICENCE)
L1 <- lrm(EVinterest ~ RAGE + Male + inc1000 +  degree, EVINTEREST)
lrm(L1)

L11 <- lrm(EVinterest ~ RAGE + Male + as.factor(inccat) + degree, 
           EVINTEREST)
lrm(L11)

stargazer::stargazer(L1, L11, 
                     title = 'Interests in EVs: base models',
                     digits = 2, type = 'text')

# Estimate model-2 (AGE & SEX & INCOME & Education & LICENCE & Employment)
L2 <- lrm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + NumCar, EVINTEREST)
lrm(L2)

# Estimate model-3 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child)
L3 <- lrm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + NumCar + NumDepCh,
          EVINTEREST)
lrm(L3)

# Estimate model-4 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child & travel freq)
L4 <- lrm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + NumCar + NumDepCh
          + Scotland,
          EVINTEREST)
lrm(L4)


### present results
stargazer::stargazer(L1, L11, L2, L3, L4, 
                     title = 'Interests in EVs',
                     digits = 2, type = 'text')



# prefered model? (AGE & SEX & INCOME & Education & LICENCE & REGION & marital & NumCar)
LPref <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + NorthWest 
             + YorkshireHumber + EastMidlands + WestMidrands + EastofEngland 
             + London + SouthEast + SouthWest + Wales + Scotland + Married 
             + MarriedSep + Divorced + Widowed + NumCar,
             data = EVINTEREST, family = 'binomial')
summary(LPref)

labelsPref <- c('Age', 'Male', 'Income (in 1000 GBP)', 'College degree', 'Valid licence', 
                'North West', 'Yorkshire and the Humber', 'East Midlands', 
                'West Midrands', 'East of England', 'London', 'South East', 
                'South West', 'Wales', 'Scotland', 'Married', 'Married-separated', 
                'Divorced', 'Widowed', '# of cars', '(Intercept)')
stargazer(LPref, covariate.labels = labelsPref,
          title = 'Interests in EVs',
          digits = 2, type = 'text')



# predicted probabilities by sex & licence (other covariates are fixed)
fittedP <- with(EVINTEREST,
                data.frame(RAGE = mean(RAGE), inc1000 = mean(inc1000, na.rm=TRUE),
                           Male = c(0,0,1,1), licence = c(0,1,0,1), degree = 1,
                           NorthWest=0, YorkshireHumber=0, EastMidlands=0, 
                           WestMidrands=0, EastofEngland=0, London=0, SouthEast=0,
                           SouthWest=0, Wales=0, Scotland=0, Married=0, MarriedSep=0,
                           Divorced=0, Widowed=0, NumCar=1))

fittedP$predicted <- predict(LPref, newdata = fittedP,
                             type = 'response')
fittedPselected <- subset(fittedP, select= c(RAGE, inc1000, Male, licence, predicted))
kable(fittedPselected, align = 'c', digits = 2,
      caption = 'Predicted Probabilities for Fitted Values')


### Zelig plot
ZP1 <- zelig(EVinterest ~ RAGE + Male + inc1000 + degree + licence + NumCar + NumDepCh
             + Scotland, 
             cite = FALSE, data = EVINTEREST, model = 'logit')
setZP1 <- setx(ZP1, RAGE = 20:80)
simZP1 <- sim(ZP1, x = setZP1)
plot(simZP1, xlab="Age", ylab="Predicted Probability", 
     main="Predicted Probability of Having an Interest in EV by Age")

ZP2 <- zelig(EVinterest ~ RAGE + Male + inc1000 + degree + licence + NumCar + NumDepCh
             + Scotland, 
             cite = FALSE, data = EVINTEREST, model = 'logit')
setZP2 <- setx(ZP2, inc1000 = 0:55)
simZP2 <- sim(ZP2, x = setZP2)
plot(simZP2)

ZP3 <- zelig(EVinterest ~ RAGE + Male + inc1000 + degree + licence + NumCar + NumDepCh
             + Scotland, 
             cite = FALSE, data = EVINTEREST, model = 'logit')
setZP3 <- setx(ZP3, NumCar = 0:3)
simZP3 <- sim(ZP3, x = setZP3)
plot(simZP3)

ZP4 <- zelig(EVinterest ~ RAGE + Male + high + lowermiddle + highermiddle + degree + licence + NumCar + NumDepCh
             + Scotland, 
             cite = FALSE, data = EVINTEREST, model = 'logit')
setZP4 <- setx(ZP4, NumDepCh = 0:7)
simZP4 <- sim(ZP4, x = setZP4)
plot(simZP4)

