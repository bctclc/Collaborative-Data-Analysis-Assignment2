### Claire & Noriko ###
### descriptive analysis ###

setwd("E:\\—ßœ∞œ‡πÿ\\R\\Collaborative-Data-Analysis-Assignment2\\datasets")

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
mtext(side=1, "Low: income below °Í14,559", line=4)
mtext(side=1, "Low-Mid: income between °Í14,560 and °Í20,799", line=5)
mtext(side=1, "High-Mid: income between °Í20,800 and °Í25,999", line=6)     
mtext(side=1, "High: income above °Í26,000", line=7)

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

mean(EVINTEREST$Scotland)
