### logit model from class 10/30 ###
### Claire & Noriko ###

library(stargazer)
library(knitr)
library(Zelig)

# set working directory (change this line)
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/datasets")

# road the cleaned up dataframe
load("clean_dataset_ver2.rda")


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


### step-wise logistic regression ###
# Estimate model-1 (AGE & SEX)
Logit1 <- glm(EVinterest ~ RAGE + Male,
              data = finaldata, family = 'binomial')
summary(Logit1)

# Estimate model-2 (AGE & SEX & INCOME)
Logit2 <- glm(EVinterest ~ RAGE + Male + inc1000,
              data = finaldata, family = 'binomial')
summary(Logit2)

# Estimate model-3 (AGE & SEX & INCOME & LICENCE)
Logit3 <- glm(EVinterest ~ RAGE + Male + inc1000 + licence,
              data = finaldata, family = 'binomial')
summary(Logit3)

### displaying the results ###
# Create cleaner covariate labels
labels <- c('Age', 'Male', 'Income (in 1000 GBP)', 'Valid licence', '(Intercept)') 

stargazer::stargazer(Logit1, Logit2, Logit3, covariate.labels = labels,
                     title = 'Interests in EVs',
                     digits = 2, type = 'html')
# the output html can be previewed online for example here: http://www.onlinehtmleditor.net/

# predicted probabilities
fitted1 <- with(finaldata,
                data.frame(RAGE = mean(RAGE),
                           inc1000 = mean(inc1000, na.rm=TRUE),
                           Male = c(0,0,1,1),
                           licence = c(0,1,0,1)))

fitted1$predicted <- predict(Logit3, newdata = fitted1,
                            type = 'response')

kable(fitted1, align = 'c', digits = 2,
      caption = 'Predicted Probabilities for Fitted Values')


### Zelig plot
# seems like 'sim' from Zelig doesn't work if any of the variables have missing values.
# create another subset EVINTEREST without missings
EVINTEREST <- subset(finaldata, select = c(EVinterest, RAGE, Male, inc1000, licence))
EVINTEREST <- na.omit(EVINTEREST)

Z1 <- zelig(EVinterest ~ RAGE + Male + inc1000 + licence, cite = FALSE,
              data = EVINTEREST, model = 'logit')
setZ1 <- setx(Z1, RAGE = 20:80)
simZ1 <- sim(Z1, x = setZ1)
plot(simZ1)
