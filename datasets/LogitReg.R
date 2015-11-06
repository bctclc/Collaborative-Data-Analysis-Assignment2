### logit models for assignment 3 ###
### Claire & Noriko ###

library(stargazer)
library(knitr)
library(Zelig)

# set working directory (change this line to your repository location)
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/datasets")

# road the cleaned up dataframe
load("EVdata1.rda")
load("EVdata2.rda")
load("EVdata3.rda")


### step-wise logistic regression ###
# Estimate model-1 (AGE & SEX & INCOME & Education & LICENCE)
L1 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence,
              data = EVINTEREST, family = 'binomial')
summary(L1)

# Estimate model-2 (AGE & SEX & INCOME & Education & LICENCE & Employment)
# Unemployed is the reference category for employment
L2 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
          + Inactive,
          data = EVINTEREST, family = 'binomial')
summary(L2)

# Estimate model-3 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child)
L3 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
          + Inactive + DVHsize + NumDepCh,
          data = EVINTEREST, family = 'binomial')
summary(L3)

# Estimate model-4 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child & travel freq)
L4 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
          + Inactive + DVHsize + NumDepCh + travfreq,
          data = EVINTEREST, family = 'binomial')
summary(L4)

# Estimate model-5 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child & travel freq & REGION)
# NorthEast is the reference category for region
L5 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
          + Inactive + DVHsize + NumDepCh + travfreq + NorthWest 
          + YorkshireHumber + EastMidlands + WestMidrands + EastofEngland 
          + London + SouthEast + SouthWest + Wales + Scotland,
          data = EVINTEREST, family = 'binomial')
summary(L5)

# Estimate model-6 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child & travel freq & REGION & marital)
# single is the reference category for marital status
L6 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
          + Inactive + DVHsize + NumDepCh + travfreq + NorthWest 
          + YorkshireHumber + EastMidlands + WestMidrands + EastofEngland 
          + London + SouthEast + SouthWest + Wales + Scotland + Married 
          + MarriedSep + Divorced + Widowed,
          data = EVINTEREST, family = 'binomial')
summary(L6)

# Estimate model-7 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child & travel freq & REGION & marital & NumCar)
L7 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
          + Inactive + DVHsize + NumDepCh + travfreq + NorthWest 
          + YorkshireHumber + EastMidlands + WestMidrands + EastofEngland 
          + London + SouthEast + SouthWest + Wales + Scotland + Married 
          + MarriedSep + Divorced + Widowed + NumCar,
          data = EVINTEREST, family = 'binomial')
summary(L7)

# Estimate model-8 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child & travel freq & REGION & marital & NumCar 
#                   & illness/disability)
L8 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
          + Inactive + DVHsize + NumDepCh + travfreq + NorthWest 
          + YorkshireHumber + EastMidlands + WestMidrands + EastofEngland 
          + London + SouthEast + SouthWest + Wales + Scotland + Married 
          + MarriedSep + Divorced + Widowed + NumCar + illness,
          data = EVINTEREST, family = 'binomial')
summary(L8)

### displaying the results ###
# Create cleaner covariate labels
labels <- c('Age', 'Male', 'Income (in 1000 GBP)', 'College degree', 'Valid licence', 
            'Eemployed', 'Economically Inactive', 'Household size', '# of Dependen Child', 
            'Travel everyday', 'North West', 'Yorkshire and the Humber', 'East Midlands', 
            'West Midrands', 'East of England', 'London', 'South East', 'South West', 
            'Wales', 'Scotland', 'Married', 'Married-separated', 'Divorced', 
            'Widowed', '# of cars', 'illness/disability', '(Intercept)') 

stargazer::stargazer(L1, L2, L3, L4, L5, L6, L7, L8, covariate.labels = labels,
                     title = 'Interests in EVs',
                     digits = 2, type = 'text')


### more models with other subsets
# Estimate model-9 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child & travel freq & REGION & marital & NumCar 
#                   & illness/disability & Full-time)
# Part-time is the reference category for work status
L9 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
          + Inactive + DVHsize + NumDepCh + travfreq + NorthWest 
          + YorkshireHumber + EastMidlands + WestMidrands + EastofEngland 
          + London + SouthEast + SouthWest + Wales + Scotland + Married 
          + MarriedSep + Divorced + Widowed + NumCar + illness + Fulltime,
          data = EVINTERESTemp, family = 'binomial')
summary(L9)
labels2 <- c('Age', 'Male', 'Income (in 1000 GBP)', 'College degree', 'Valid licence', 
             'Eemployed', 'Economically Inactive', 'Household size', '# of Dependen Child', 
             'Travel everyday', 'North West', 'Yorkshire and the Humber', 'East Midlands', 
             'West Midrands', 'East of England', 'London', 'South East', 'South West', 
             'Wales', 'Scotland', 'Married', 'Married-separated', 'Divorced', 
             'Widowed', '# of cars', 'illness/disability', 'Full-time', '(Intercept)') 
stargazer::stargazer(L1, L2, L3, L4, L5, L6, L7, L8, L9, covariate.labels = labels2,
                     title = 'Interests in EVs',
                     digits = 2, type = 'text')

# Estimate model-10 (AGE & SEX & INCOME & Education & LICENCE & Employment 
#                   & HHsize & Child & travel freq & REGION & marital & NumCar 
#                   & limiting illness/disability)
# Part-time is the reference category for work status
L10 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + Employed 
           + Inactive + DVHsize + NumDepCh + travfreq + NorthWest 
           + YorkshireHumber + EastMidlands + WestMidrands + EastofEngland 
           + London + SouthEast + SouthWest + Wales + Scotland + Married 
           + MarriedSep + Divorced + Widowed + NumCar + illnesslim,
           data = EVINTERESTill, family = 'binomial')
summary(L10)
labels3 <- c('Age', 'Male', 'Income (in 1000 GBP)', 'College degree', 'Valid licence', 
             'Eemployed', 'Economically Inactive', 'Household size', '# of Dependen Child', 
             'Travel everyday', 'North West', 'Yorkshire and the Humber', 'East Midlands', 
             'West Midrands', 'East of England', 'London', 'South East', 'South West', 
             'Wales', 'Scotland', 'Married', 'Married-separated', 'Divorced', 
             'Widowed', '# of cars', 'limiting illness/disability', '(Intercept)') 
stargazer::stargazer(L10, covariate.labels = labels3,
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
ZP <- zelig(EVinterest ~ RAGE + Male + inc1000 + degree + licence + NorthWest 
            + YorkshireHumber + EastMidlands + WestMidrands + EastofEngland 
            + London + SouthEast + SouthWest + Wales + Scotland + Married 
            + MarriedSep + Divorced + Widowed + NumCar,
            cite = FALSE, data = EVINTEREST, model = 'logit')
setZP <- setx(ZP, RAGE = 20:80)
simZP <- sim(ZP, x = setZP)
plot(simZP)

