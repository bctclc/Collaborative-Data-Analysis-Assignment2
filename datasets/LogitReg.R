### logit model for assignment 3 ###
### Claire & Noriko ###

library(stargazer)
library(knitr)
library(Zelig)

# set working directory (change this line to your repository location)
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/datasets")

# road the cleaned up dataframe
load("clean_dataset.rda")


### a bit more clean up to make the output tydier ###
# recode sex variable into 0/1
finaldata$Male <- 0
finaldata$Male[finaldata$RSEX==1] <- 1
table(finaldata$RSEX, finaldata$Male)

# income into 1000GBP
finaldata$inc1000 <- finaldata$income/1000

# having a valid driver's licence
finaldata$licence[finaldata$MEC_1<=2] <- 1
finaldata$licence[finaldata$MEC_1==3] <- 0

# travel everyday
finaldata$travfreq[finaldata$MEC_3==1] <- 1
finaldata$travfreq[finaldata$MEC_3>=2 & finaldata$MEC_3<=8] <- 0
table(finaldata$travfreq)

# degree
finaldata$degree[finaldata$HighEd1==1] <- 1
finaldata$degree[finaldata$HighEd1>=2 & finaldata$HighEd1<=8] <- 0
table(finaldata$degree)


### step-wise logistic regression ###
# Estimate model-1 (AGE & SEX & INCOME & Education & LICENCE)
L1 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence,
              data = finaldata, family = 'binomial')
summary(L1)

# Estimate model-2 (AGE & SEX & INCOME & Education & LICENCE & Employment)
L2 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(DVILO3a),
          data = finaldata, family = 'binomial')
summary(L2)

# Estimate model-3 (AGE & SEX & INCOME & Education & LICENCE & Employment & HHsize 
#                   & Child)
L3 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(DVILO3a) 
          + DVHsize + NumDepCh,
          data = finaldata, family = 'binomial')
summary(L3)

# Estimate model-4 (AGE & SEX & INCOME & Education & LICENCE & Employment & HHsize 
#                   & Child & travel freq)
L4 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(DVILO3a) 
          + DVHsize + NumDepCh + travfreq,
          data = finaldata, family = 'binomial')
summary(L4)

# Estimate model-5 (AGE & SEX & INCOME & Education & LICENCE & Employment & HHsize 
#                   & Child & travel freq & REGION)
L5 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(DVILO3a) 
          + DVHsize + NumDepCh + travfreq + as.factor(GorA),
          data = finaldata, family = 'binomial')
summary(L5)

# Estimate model-6 (AGE & SEX & INCOME & Education & LICENCE & Employment & HHsize 
#                   & Child & travel freq & REGION & work status)
L6 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(DVILO3a) 
          + DVHsize + NumDepCh + travfreq + as.factor(GorA) + as.factor(FtPtWk),
          data = finaldata, family = 'binomial')
summary(L6)

# Estimate model-7 (AGE & SEX & INCOME & Education & LICENCE & Employment & HHsize 
#                   & Child & travel freq & REGION & marital)
L7 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(DVILO3a) 
          + DVHsize + NumDepCh + travfreq + as.factor(GorA) + as.factor(Respmar),
          data = finaldata, family = 'binomial')
summary(L7)

# Estimate model-8 (AGE & SEX & INCOME & Education & LICENCE & Employment & HHsize 
#                   & Child & travel freq & REGION & marital & car abailability)
L8 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(DVILO3a) 
          + DVHsize + NumDepCh + travfreq + as.factor(GorA) + as.factor(Respmar) + CAR,
          data = finaldata, family = 'binomial')
summary(L8)

# Estimate model-9 (AGE & SEX & INCOME & Education & LICENCE & Employment & HHsize 
#                   & Child & travel freq & REGION & marital & car abailability & Disability)
L9 <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(DVILO3a) 
          + DVHsize + NumDepCh + travfreq + as.factor(GorA) + as.factor(Respmar) + CAR 
          + as.factor(IllLim),
          data = finaldata, family = 'binomial')
summary(L9)

### displaying the results ###
# Create cleaner covariate labels
labels <- c('Age', 'Male', 'Income (in 1000 GBP)', 'College degree', 'Valid licence', 
            'Unemployed', 'Economically Inactive', 'Household size', 'Dependen Child', 
            'Travel everyday', 'North West', 'Yorkshire and the Humber', 'East Midlands', 
            'West Midrands', 'East of England', 'London', 'South East', 'South West', 
            'Wales', 'Scotland', 'Part-time', 'Married', 'Married-separated', 'Divorced', 
            'Widowed', 'Car', 'No Illness/Disability', '(Intercept)') 

stargazer::stargazer(L1, L2, L3, L4, L5, L6, L7, L8, L9, covariate.labels = labels,
                     title = 'Interests in EVs',
                     digits = 2, type = 'text')


# Selected variables? (AGE & MALE & INCOME & DEGREE & LICENCE & MARITAL)
LP <- glm(EVinterest ~ RAGE + Male + inc1000 + degree + licence + as.factor(Respmar),
          data = finaldata, family = 'binomial')
labelsP <- c('Age', 'Male', 'Income (in 1000 GBP)', 'College degree', 'Valid licence', 
             'Married', 'Married-separated', 'Divorced', 'Widowed', '(Intercept)')
stargazer(LP, covariate.labels = labelsP,
          title = 'Interests in EVs',
          digits = 2, type = 'text')


# predicted probabilities
fittedP <- with(finaldata,
                data.frame(RAGE = mean(RAGE),
                           inc1000 = mean(inc1000, na.rm=TRUE),
                           Male = c(0,0,1,1),
                           licence = c(0,1,0,1),
                           degree = 1,
                           Respmar = 1))

fittedP$predicted <- predict(LP, newdata = fittedP,
                             type = 'response')

kable(fittedP, align = 'c', digits = 2,
      caption = 'Predicted Probabilities for Fitted Values')


### Zelig plot
# seems like 'sim' from Zelig doesn't work if any of the variables have missing values.
# create another subset EVINTEREST without missings
EVINTEREST <- subset(finaldata, select = c(EVinterest, RAGE, Male, inc1000, degree, 
                                           licence, Respmar))
EVINTEREST <- na.omit(EVINTEREST)

# seems like as.factor() cannot be used in zelig...
ZP <- zelig(EVinterest ~ RAGE + Male + inc1000 + degree + licence, 
            cite = FALSE, data = EVINTEREST, model = 'logit')
setZP <- setx(ZP, RAGE = 20:80)
simZP <- sim(ZP, x = setZP)
plot(simZP)

