### Claire & Noriko ###
### descriptive analysis ###

setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Assignment3")

load("EVdata1.rda")

mean(EVINTEREST$EVinterest)

summary(EVINTEREST$RAGE)
hist(EVINTEREST$RAGE,
     main="Age Distribution", xlab="Age", ylab="Frequency", 
     xlim=c(10,100))
axis(side=1, at=seq(0,100, 10))

mean(EVINTEREST$Male)

summary(EVINTEREST$inccat)
inctable <-table(EVINTEREST$inccat)
barplot(inctable, main="Income Distribtion",ylab="Frequnecy",
        names.arg=c("Low", "Low-Mid", "High-Mid", "High"))
mtext(side=1, "Gross Annual Income", line=2)
mtext(side=1, "(Low:~14,559 / Low-Mid:14,560~20,799 / High-Mid:20,800~25,999 / High: 26,000+)", line=3)


mean(degree)

mean(license)

summary(EVINTEREST$NumCar)
cartable <-table(EVINTEREST$NumCar)
barplot(cartable, main="Number of Cars in Each Household", 
     xlab="Number of Cars", ylab="Frequency")

summary(EVINTEREST$NumDepCh)
childtable <- table(EVINTEREST$NumDepCh)
barplot(childtable, main="Number of Dependent Children in Each Household", 
        xlab="Number of Children", ylab="Frequency")

summary(EVINTEREST$DVHsize)
Hsizetable <- table(EVINTEREST$DVHsize)
barplot(Hsizetable, main="Household size", 
        xlab="Household size", ylab="Frequency")

mean(EVINTEREST$Scotland)

mean(EVINTEREST$havechildren)
