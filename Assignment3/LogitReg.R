### logit models for assignment 3 ###
### Claire & Noriko ###

### set working directory
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Assignment3")

library(stargazer)
library(knitr)
library(Zelig)
library(rms)



# road the cleaned up dataframe
load("EVdata1.rda")
load("EVdata2.rda")
load("EVdata3.rda")


### step-wise logistic regression ###
# Estimate model-1 (AGE & SEX & INCOME & Education)
# Categorical income (4 classes, low is the reference)
L1 <- lrm(EVinterest ~ RAGE + Male + lowermiddle + highermiddle + high + degree, 
           EVINTEREST)
lrm(L1)


# Estimate model-2 (AGE & SEX & INCOME & Education & Licence & # of Cars)
L2 <- lrm(EVinterest ~ RAGE + Male + lowermiddle + highermiddle + high + degree 
          + licence + NumCar, EVINTEREST)
lrm(L2)

# Estimate model-3 (AGE & SEX & INCOME & Education & Licence & # of Cars & # of children)
L3 <- lrm(EVinterest ~ RAGE + Male + lowermiddle + highermiddle + high + degree 
          + licence + NumCar + DVHsize + havechildren, EVINTEREST)
lrm(L3)

# Estimate model-4 (AGE & SEX & INCOME & Education & Licence & # of Cars & # of children & Region)
L4 <- lrm(EVinterest ~ RAGE + Male + lowermiddle + highermiddle + high + degree
          + licence + NumCar + DVHsize + havechildren + Scotland, EVINTEREST)
lrm(L4)


### present results
# Create cleaner covariate labels
labels <- c('Age', 'Male', 'Income: low-middle', 'Income: high-middle', 'Income: high', 
            'College degree', 'Drivers licence', '# of cars', "Size of household", 'Having dependent children', 
            'Scotland', '(Intercept)') 
stargazer::stargazer(L1, L2, L3, L4, covariate.labels = labels,
                     title = 'Interests in EVs',
                     digits = 2, type = 'text')


### predicted probabilities by income (other covariates are fixed)
L4Pred <- glm(EVinterest ~ RAGE + Male + lowermiddle + highermiddle + high + degree
              + licence + NumCar + NumDepCh + Scotland, 
              data = EVINTEREST, family = 'binomial')
fittedP <- with(EVINTEREST,
                data.frame(RAGE=mean(RAGE), Male=c(1,1,1,1), inccat=c(1,2,3,4), 
                           low=c(1,0,0,0), lowermiddle=c(0,1,0,0), highermiddle=c(0,0,1,0), 
                           high=c(0,0,0,1), licence=1, degree =1, NumCar=1,
                           NumDepCh=1, Scotland=0))
fittedP$predicted <- predict(L4Pred, newdata = fittedP,
                             type = 'response')
fittedPselected <- subset(fittedP, select= c(inccat, predicted))
kable(fittedPselected, align = 'c', digits = 2,
      caption = 'Predicted Probabilities for Fitted Values')


### Zelig plot
ZP <- zelig(EVinterest ~ RAGE + Male + lowermiddle + highermiddle + high 
             + degree + licence + NumCar + NumDepCh + Scotland, 
             cite = FALSE, data = EVINTEREST, model = 'logit')
setZP1 <- setx(ZP, RAGE = 20:80)
simZP1 <- sim(ZP, x = setZP1)
plot(simZP1, xlab="Age", ylab="Predicted Probability", 
     main="Predicted Probability of Having an Interest in EV by Age")

setZP2 <- setx(ZP, NumCar = 0:3)
simZP2 <- sim(ZP, x = setZP2)
plot(simZP2, xlab="Number of Cars in Household", ylab="Predicted Probability", 
     main="Predicted Probability of Having an Interest in EV by # of Cars")

setZP3 <- setx(ZP, NumDepCh = 0:7)
simZP3 <- sim(ZP, x = setZP3)
plot(simZP3, xlab="Number of Dependent Childen", ylab="Predicted Probability", 
     main="Predicted Probability of Having an Interest in EV by # of Children")

