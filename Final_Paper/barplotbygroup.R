possible_wd <- c("E:\\Ñ§Ï°Ïà¹Ø\\R\\Collaborative-Data-Analysis-Assignment2\\Final_Paper", "C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Final_Paper")
repmis::set_valid_wd(possible_wd)

library(stargazer)
library(knitr)
library(Zelig)
library(rms)
library(captioner)
library(foreign)
library(dplyr)
library(ggplot2)
library(googleVis)
library(gridExtra)

source("data_cleanup.R")

#age group & factors encouraging buying
agemean <- c(mean(EVINTEREST$ENcost[EVINTEREST$agegroup==1]),
             mean(EVINTEREST$ENbattery[EVINTEREST$agegroup==1]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$agegroup==1]),
             mean(EVINTEREST$ENresale[EVINTEREST$agegroup==1]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$agegroup==1]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$agegroup==1]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$agegroup==1]),
             mean(EVINTEREST$ENtech[EVINTEREST$agegroup==1]),
             mean(EVINTEREST$ENenv[EVINTEREST$agegroup==1]),
             mean(EVINTEREST$ENcost[EVINTEREST$agegroup==2]),
             mean(EVINTEREST$ENbattery[EVINTEREST$agegroup==2]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$agegroup==2]),
             mean(EVINTEREST$ENresale[EVINTEREST$agegroup==2]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$agegroup==2]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$agegroup==2]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$agegroup==2]),
             mean(EVINTEREST$ENtech[EVINTEREST$agegroup==2]),
             mean(EVINTEREST$ENenv[EVINTEREST$agegroup==2]),
             mean(EVINTEREST$ENcost[EVINTEREST$agegroup==3]),
             mean(EVINTEREST$ENbattery[EVINTEREST$agegroup==3]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$agegroup==3]),
             mean(EVINTEREST$ENresale[EVINTEREST$agegroup==3]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$agegroup==3]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$agegroup==3]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$agegroup==3]),
             mean(EVINTEREST$ENtech[EVINTEREST$agegroup==3]),
             mean(EVINTEREST$ENenv[EVINTEREST$agegroup==3]))
ENfactor3 <- c("cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism")
Agegroupl <- c(1,1,1,1,1,1,1,1,1,
               2,2,2,2,2,2,2,2,2,
               3,3,3,3,3,3,3,3,3)
meanagetable <- cbind(agemean, ENfactor3, Agegroupl)
ageEN <- ggplot(as.data.frame(meanagetable), aes(factor(ENfactor3), as.numeric(agemean), fill = as.factor(Agegroupl))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("young people", "middle-age people", "elder people"), 
                    values=c("dodgerblue", "dodgerblue2", "dodgerblue3")) +
  scale_x_discrete("  ",limits=c("cost", "battery", "recharge", "environmentalism", "technology", "design","choice", "safety", "resale")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#sex & factors encouraging buying
sexmean <- c(mean(EVINTEREST$ENcost[EVINTEREST$Male==1]),
             mean(EVINTEREST$ENbattery[EVINTEREST$Male==1]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$Male==1]),
             mean(EVINTEREST$ENresale[EVINTEREST$Male==1]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$Male==1]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$Male==1]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$Male==1]),
             mean(EVINTEREST$ENtech[EVINTEREST$Male==1]),
             mean(EVINTEREST$ENenv[EVINTEREST$Male==1]),
             mean(EVINTEREST$ENcost[EVINTEREST$Male==0]),
             mean(EVINTEREST$ENbattery[EVINTEREST$Male==0]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$Male==0]),
             mean(EVINTEREST$ENresale[EVINTEREST$Male==0]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$Male==0]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$Male==0]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$Male==0]),
             mean(EVINTEREST$ENtech[EVINTEREST$Male==0]),
             mean(EVINTEREST$ENenv[EVINTEREST$Male==0]))
ENfactor2 <- c("cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism")
sexgroupl <- c(1,1,1,1,1,1,1,1,1,
               2,2,2,2,2,2,2,2,2)
meansextable <- cbind(sexmean, ENfactor2, sexgroupl)
sexEN <- ggplot(as.data.frame(meansextable), aes(factor(ENfactor2), as.numeric(sexmean), fill = as.factor(sexgroupl))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("male", "female"), 
                    values=c("dodgerblue", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("cost", "battery", "recharge", "environmentalism", "technology", "design","choice", "safety", "resale")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#income & factors encouraging buying
incmean <- c(mean(EVINTEREST$ENcost[EVINTEREST$inccat==1]),
             mean(EVINTEREST$ENbattery[EVINTEREST$inccat==1]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$inccat==1]),
             mean(EVINTEREST$ENresale[EVINTEREST$inccat==1]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$inccat==1]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$inccat==1]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$inccat==1]),
             mean(EVINTEREST$ENtech[EVINTEREST$inccat==1]),
             mean(EVINTEREST$ENenv[EVINTEREST$inccat==1]),
             mean(EVINTEREST$ENcost[EVINTEREST$inccat==2]),
             mean(EVINTEREST$ENbattery[EVINTEREST$inccat==2]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$inccat==2]),
             mean(EVINTEREST$ENresale[EVINTEREST$inccat==2]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$inccat==2]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$inccat==2]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$inccat==2]),
             mean(EVINTEREST$ENtech[EVINTEREST$inccat==2]),
             mean(EVINTEREST$ENenv[EVINTEREST$inccat==2]),
             mean(EVINTEREST$ENcost[EVINTEREST$inccat==3]),
             mean(EVINTEREST$ENbattery[EVINTEREST$inccat==3]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$inccat==3]),
             mean(EVINTEREST$ENresale[EVINTEREST$inccat==3]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$inccat==3]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$inccat==3]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$inccat==3]),
             mean(EVINTEREST$ENtech[EVINTEREST$inccat==3]),
             mean(EVINTEREST$ENenv[EVINTEREST$inccat==3]),
             mean(EVINTEREST$ENcost[EVINTEREST$inccat==4]),
             mean(EVINTEREST$ENbattery[EVINTEREST$inccat==4]), 
             mean(EVINTEREST$ENrecharge[EVINTEREST$inccat==4]),
             mean(EVINTEREST$ENresale[EVINTEREST$inccat==4]), 
             mean(EVINTEREST$ENsafety[EVINTEREST$inccat==4]),
             mean(EVINTEREST$ENcarspec[EVINTEREST$inccat==4]), 
             mean(EVINTEREST$ENchoice[EVINTEREST$inccat==4]),
             mean(EVINTEREST$ENtech[EVINTEREST$inccat==4]),
             mean(EVINTEREST$ENenv[EVINTEREST$inccat==4]))
ENfactor4 <- c("cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism")
incgroupl <- c(1,1,1,1,1,1,1,1,1,
               2,2,2,2,2,2,2,2,2,
               3,3,3,3,3,3,3,3,3,
               4,4,4,4,4,4,4,4,4)
meaninctable <- cbind(incmean, ENfactor4, incgroupl)
incomeEN <- ggplot(as.data.frame(meaninctable), aes(factor(ENfactor4), as.numeric(incmean), fill = as.factor(incgroupl))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("low-income \n people", "lower-middle-income \n people", "higher-middle-income \n people", "high-income \n people"), 
                    values=c("dodgerblue", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("cost", "battery", "recharge", "environmentalism", "technology", "design","choice", "safety", "resale")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#college & factors encouraging buying
collegemean <- c(mean(EVINTEREST$ENcost[EVINTEREST$degree==1]),
                 mean(EVINTEREST$ENbattery[EVINTEREST$degree==1]), 
                 mean(EVINTEREST$ENrecharge[EVINTEREST$degree==1]),
                 mean(EVINTEREST$ENresale[EVINTEREST$degree==1]), 
                 mean(EVINTEREST$ENsafety[EVINTEREST$degree==1]),
                 mean(EVINTEREST$ENcarspec[EVINTEREST$degree==1]), 
                 mean(EVINTEREST$ENchoice[EVINTEREST$degree==1]),
                 mean(EVINTEREST$ENtech[EVINTEREST$degree==1]),
                 mean(EVINTEREST$ENenv[EVINTEREST$degree==1]),
                 mean(EVINTEREST$ENcost[EVINTEREST$degree==0]),
                 mean(EVINTEREST$ENbattery[EVINTEREST$degree==0]), 
                 mean(EVINTEREST$ENrecharge[EVINTEREST$degree==0]),
                 mean(EVINTEREST$ENresale[EVINTEREST$degree==0]), 
                 mean(EVINTEREST$ENsafety[EVINTEREST$degree==0]),
                 mean(EVINTEREST$ENcarspec[EVINTEREST$degree==0]), 
                 mean(EVINTEREST$ENchoice[EVINTEREST$degree==0]),
                 mean(EVINTEREST$ENtech[EVINTEREST$degree==0]),
                 mean(EVINTEREST$ENenv[EVINTEREST$degree==0]))
ENfactor2 <- c("cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism")
sexgroupl <- c(1,1,1,1,1,1,1,1,1,
               2,2,2,2,2,2,2,2,2)
collegemeantable <- cbind(collegemean, ENfactor2, sexgroupl)
collegeEN <- ggplot(as.data.frame(collegemeantable), aes(factor(ENfactor2), as.numeric(collegemean), fill = as.factor(sexgroupl))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("People with \n college degree", "people without \n college degree"), 
                    values=c("dodgerblue", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("cost", "battery", "recharge", "environmentalism", "technology", "design","choice", "safety", "resale")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#driver's licence & factors encouraging buying
licencemean <- c(mean(EVINTEREST$ENcost[EVINTEREST$licence==1]),
                 mean(EVINTEREST$ENbattery[EVINTEREST$licence==1]), 
                 mean(EVINTEREST$ENrecharge[EVINTEREST$licence==1]),
                 mean(EVINTEREST$ENresale[EVINTEREST$licence==1]), 
                 mean(EVINTEREST$ENsafety[EVINTEREST$licence==1]),
                 mean(EVINTEREST$ENcarspec[EVINTEREST$licence==1]), 
                 mean(EVINTEREST$ENchoice[EVINTEREST$licence==1]),
                 mean(EVINTEREST$ENtech[EVINTEREST$licence==1]),
                 mean(EVINTEREST$ENenv[EVINTEREST$licence==1]),
                 mean(EVINTEREST$ENcost[EVINTEREST$licence==0]),
                 mean(EVINTEREST$ENbattery[EVINTEREST$licence==0]), 
                 mean(EVINTEREST$ENrecharge[EVINTEREST$licence==0]),
                 mean(EVINTEREST$ENresale[EVINTEREST$licence==0]), 
                 mean(EVINTEREST$ENsafety[EVINTEREST$licence==0]),
                 mean(EVINTEREST$ENcarspec[EVINTEREST$licence==0]), 
                 mean(EVINTEREST$ENchoice[EVINTEREST$licence==0]),
                 mean(EVINTEREST$ENtech[EVINTEREST$licence==0]),
                 mean(EVINTEREST$ENenv[EVINTEREST$licence==0]))
ENfactor2 <- c("cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism")
sexgroupl <- c(1,1,1,1,1,1,1,1,1,
               2,2,2,2,2,2,2,2,2)
licencemeantable <- cbind(licencemean, ENfactor2, sexgroupl)
licenceEN <- ggplot(as.data.frame(licencemeantable), aes(factor(ENfactor2), as.numeric(licencemean), fill = as.factor(sexgroupl))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("People with \n driver's licence", "people without \n driver's licence"), 
                    values=c("dodgerblue", "dodgerblue4")) +
  scale_x_discrete("  ", limits=c("cost", "battery", "recharge", "environmentalism", "technology", "design","choice", "safety", "resale")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#number of cars & factors encouraging buying
carnomean <- c(mean(EVINTEREST$ENcost[EVINTEREST$NumCar==0]),
               mean(EVINTEREST$ENbattery[EVINTEREST$NumCar==0]), 
               mean(EVINTEREST$ENrecharge[EVINTEREST$NumCar==0]),
               mean(EVINTEREST$ENresale[EVINTEREST$NumCar==0]), 
               mean(EVINTEREST$ENsafety[EVINTEREST$NumCar==0]),
               mean(EVINTEREST$ENcarspec[EVINTEREST$NumCar==0]), 
               mean(EVINTEREST$ENchoice[EVINTEREST$NumCar==0]),
               mean(EVINTEREST$ENtech[EVINTEREST$NumCar==0]),
               mean(EVINTEREST$ENenv[EVINTEREST$NumCar==0]),
               mean(EVINTEREST$ENcost[EVINTEREST$NumCar==1]),
               mean(EVINTEREST$ENbattery[EVINTEREST$NumCar==1]), 
               mean(EVINTEREST$ENrecharge[EVINTEREST$NumCar==1]),
               mean(EVINTEREST$ENresale[EVINTEREST$NumCar==1]), 
               mean(EVINTEREST$ENsafety[EVINTEREST$NumCar==1]),
               mean(EVINTEREST$ENcarspec[EVINTEREST$NumCar==1]), 
               mean(EVINTEREST$ENchoice[EVINTEREST$NumCar==1]),
               mean(EVINTEREST$ENtech[EVINTEREST$NumCar==1]),
               mean(EVINTEREST$ENenv[EVINTEREST$NumCar==1]),
               mean(EVINTEREST$ENcost[EVINTEREST$NumCar==2]),
               mean(EVINTEREST$ENbattery[EVINTEREST$NumCar==2]), 
               mean(EVINTEREST$ENrecharge[EVINTEREST$NumCar==2]),
               mean(EVINTEREST$ENresale[EVINTEREST$NumCar==2]), 
               mean(EVINTEREST$ENsafety[EVINTEREST$NumCar==2]),
               mean(EVINTEREST$ENcarspec[EVINTEREST$NumCar==2]), 
               mean(EVINTEREST$ENchoice[EVINTEREST$NumCar==2]),
               mean(EVINTEREST$ENtech[EVINTEREST$NumCar==2]),
               mean(EVINTEREST$ENenv[EVINTEREST$NumCar==2]),
               mean(EVINTEREST$ENcost[EVINTEREST$NumCar==3]),
               mean(EVINTEREST$ENbattery[EVINTEREST$NumCar==3]), 
               mean(EVINTEREST$ENrecharge[EVINTEREST$NumCar==3]),
               mean(EVINTEREST$ENresale[EVINTEREST$NumCar==3]), 
               mean(EVINTEREST$ENsafety[EVINTEREST$NumCar==3]),
               mean(EVINTEREST$ENcarspec[EVINTEREST$NumCar==3]), 
               mean(EVINTEREST$ENchoice[EVINTEREST$NumCar==3]),
               mean(EVINTEREST$ENtech[EVINTEREST$NumCar==3]),
               mean(EVINTEREST$ENenv[EVINTEREST$NumCar==3]))
ENfactor4 <- c("cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism")
incgroupl <- c(1,1,1,1,1,1,1,1,1,
               2,2,2,2,2,2,2,2,2,
               3,3,3,3,3,3,3,3,3,
               4,4,4,4,4,4,4,4,4)
carnomeantable <- cbind(carnomean, ENfactor4, incgroupl)
carnoEN <- ggplot(as.data.frame(carnomeantable), aes(factor(ENfactor4), as.numeric(carnomean), fill = as.factor(incgroupl))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("no car", "1 car", "2 cars", "3 cars \n or more"), 
                    values=c("dodgerblue", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("cost", "battery", "recharge", "environmentalism", "technology", "design","choice", "safety", "resale")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#in scotland & factors encouraging buying
scotmean <- c(mean(EVINTEREST$ENcost[EVINTEREST$Scotland==1]),
              mean(EVINTEREST$ENbattery[EVINTEREST$Scotland==1]), 
              mean(EVINTEREST$ENrecharge[EVINTEREST$Scotland==1]),
              mean(EVINTEREST$ENresale[EVINTEREST$Scotland==1]), 
              mean(EVINTEREST$ENsafety[EVINTEREST$Scotland==1]),
              mean(EVINTEREST$ENcarspec[EVINTEREST$Scotland==1]), 
              mean(EVINTEREST$ENchoice[EVINTEREST$Scotland==1]),
              mean(EVINTEREST$ENtech[EVINTEREST$Scotland==1]),
              mean(EVINTEREST$ENenv[EVINTEREST$Scotland==1]),
              mean(EVINTEREST$ENcost[EVINTEREST$Scotland==0]),
              mean(EVINTEREST$ENbattery[EVINTEREST$Scotland==0]), 
              mean(EVINTEREST$ENrecharge[EVINTEREST$Scotland==0]),
              mean(EVINTEREST$ENresale[EVINTEREST$Scotland==0]), 
              mean(EVINTEREST$ENsafety[EVINTEREST$Scotland==0]),
              mean(EVINTEREST$ENcarspec[EVINTEREST$Scotland==0]), 
              mean(EVINTEREST$ENchoice[EVINTEREST$Scotland==0]),
              mean(EVINTEREST$ENtech[EVINTEREST$Scotland==0]),
              mean(EVINTEREST$ENenv[EVINTEREST$Scotland==0]))
ENfactor2 <- c("cost","battery","recharge","resale","safety","design","choice","technology","environmentalism","cost","battery","recharge","resale","safety","design","choice","technology","environmentalism")
sexgroupl <- c(1,1,1,1,1,1,1,1,1,
               2,2,2,2,2,2,2,2,2)
scotmeantable <- cbind(scotmean, ENfactor2, sexgroupl)
scotEN <- ggplot(as.data.frame(scotmeantable), aes(factor(ENfactor2), as.numeric(scotmean), fill = as.factor(sexgroupl))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("people in Scotland", "people elsewhere"), 
                    values=c("dodgerblue", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("cost", "battery", "recharge", "environmentalism", "technology", "design","choice", "safety", "resale")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#put them in one

grid.arrange(ageEN, sexEN, incomeEN, collegeEN, licenceEN,carnoEN,scotEN, ncol=2, nrow=4)




#age group & factors encouraging buying
agemeanc <- c(mean(EVINTEREST$ENCpurchase[EVINTEREST$agegroup==1]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$agegroup==1]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$agegroup==1]),
              mean(EVINTEREST$ENCresale[EVINTEREST$agegroup==1]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$agegroup==1]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$agegroup==1]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$agegroup==1]),
              mean(EVINTEREST$ENCpurchase[EVINTEREST$agegroup==2]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$agegroup==2]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$agegroup==2]),
              mean(EVINTEREST$ENCresale[EVINTEREST$agegroup==2]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$agegroup==2]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$agegroup==2]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$agegroup==2]),
              mean(EVINTEREST$ENCpurchase[EVINTEREST$agegroup==3]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$agegroup==3]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$agegroup==3]),
              mean(EVINTEREST$ENCresale[EVINTEREST$agegroup==3]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$agegroup==3]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$agegroup==3]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$agegroup==3]))
ENCfactor3 <- c("purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance", "purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance", "purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance")
Agegrouplc <- c(1,1,1,1,1,1,1,
                2,2,2,2,2,2,2,
                3,3,3,3,3,3,3)
meancagetable <- cbind(agemeanc, ENCfactor3, Agegrouplc)
ageENC <- ggplot(as.data.frame(meancagetable), aes(factor(ENCfactor3), as.numeric(agemeanc), fill = as.factor(Agegrouplc))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("young people", "middle-age people", "elder people"), 
                    values=c("dodgerblue", "dodgerblue2", "dodgerblue3")) +
  scale_x_discrete("  ",limits=c("purchase", "fuel/recharging", "maintenance", "insurance", "excise tax", "resale","company tax")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#sex & factors encouraging buying
sexmeanc <- c(mean(EVINTEREST$ENCpurchase[EVINTEREST$Male==1]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$Male==1]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$Male==1]),
              mean(EVINTEREST$ENCresale[EVINTEREST$Male==1]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$Male==1]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$Male==1]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$Male==1]),
              mean(EVINTEREST$ENCpurchase[EVINTEREST$Male==0]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$Male==0]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$Male==0]),
              mean(EVINTEREST$ENCresale[EVINTEREST$Male==0]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$Male==0]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$Male==0]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$Male==0]))
ENCfactor2 <- c("purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance", "purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance")
sexgrouplc <- c(1,1,1,1,1,1,1,
                2,2,2,2,2,2,2)
meancsextable <- cbind(sexmeanc, ENCfactor2, sexgrouplc)
sexENC <- ggplot(as.data.frame(meancsextable), aes(factor(ENCfactor2), as.numeric(sexmeanc), fill = as.factor(sexgrouplc))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("male", "female"), 
                    values=c("dodgerblue", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("purchase", "fuel/recharging", "maintenance", "insurance", "excise tax", "resale","company tax")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#income & factors encouraging buying
incmeanc <- c(mean(EVINTEREST$ENCpurchase[EVINTEREST$inccat==1]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$inccat==1]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$inccat==1]),
              mean(EVINTEREST$ENCresale[EVINTEREST$inccat==1]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$inccat==1]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$inccat==1]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$inccat==1]),
              mean(EVINTEREST$ENCpurchase[EVINTEREST$inccat==2]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$inccat==2]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$inccat==2]),
              mean(EVINTEREST$ENCresale[EVINTEREST$inccat==2]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$inccat==2]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$inccat==2]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$inccat==2]),
              mean(EVINTEREST$ENCpurchase[EVINTEREST$inccat==3]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$inccat==3]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$inccat==3]),
              mean(EVINTEREST$ENCresale[EVINTEREST$inccat==3]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$inccat==3]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$inccat==3]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$inccat==3]),
              mean(EVINTEREST$ENCpurchase[EVINTEREST$inccat==4]),
              mean(EVINTEREST$ENCmaintenance[EVINTEREST$inccat==4]), 
              mean(EVINTEREST$ENCfuel[EVINTEREST$inccat==4]),
              mean(EVINTEREST$ENCresale[EVINTEREST$inccat==4]), 
              mean(EVINTEREST$ENCextax[EVINTEREST$inccat==4]),
              mean(EVINTEREST$ENCcomptac[EVINTEREST$inccat==4]), 
              mean(EVINTEREST$ENCinsurance[EVINTEREST$inccat==4]))
ENCfactor4 <- c("purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance","purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance","purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance","purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance")
incgrouplc <- c(1,1,1,1,1,1,1,
                2,2,2,2,2,2,2,
                3,3,3,3,3,3,3,
                4,4,4,4,4,4,4)
meancinctable <- cbind(incmeanc, ENCfactor4, incgrouplc)
incomeENC <- ggplot(as.data.frame(meancinctable), aes(factor(ENCfactor4), as.numeric(incmeanc), fill = as.factor(incgrouplc))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("low-income \n people", "lower-middle-income \n people", "higher-middle-income \n people", "high-income \n people"), 
                    values=c("dodgerblue", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("purchase", "fuel/recharging", "maintenance", "insurance", "excise tax", "resale","company tax")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous("  ", labels=NULL)

#college & factors encouraging buying
collegemeanc <- c(mean(EVINTEREST$ENCpurchase[EVINTEREST$degree==1]),
                  mean(EVINTEREST$ENCmaintenance[EVINTEREST$degree==1]), 
                  mean(EVINTEREST$ENCfuel[EVINTEREST$degree==1]),
                  mean(EVINTEREST$ENCresale[EVINTEREST$degree==1]), 
                  mean(EVINTEREST$ENCextax[EVINTEREST$degree==1]),
                  mean(EVINTEREST$ENCcomptac[EVINTEREST$degree==1]), 
                  mean(EVINTEREST$ENCinsurance[EVINTEREST$degree==1]),
                  mean(EVINTEREST$ENCpurchase[EVINTEREST$degree==0]),
                  mean(EVINTEREST$ENCmaintenance[EVINTEREST$degree==0]), 
                  mean(EVINTEREST$ENCfuel[EVINTEREST$degree==0]),
                  mean(EVINTEREST$ENCresale[EVINTEREST$degree==0]), 
                  mean(EVINTEREST$ENCextax[EVINTEREST$degree==0]),
                  mean(EVINTEREST$ENCcomptac[EVINTEREST$degree==0]), 
                  mean(EVINTEREST$ENCinsurance[EVINTEREST$degree==0]))
ENCfactor2 <- c("purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance","purchase","maintenance","fuel/recharging","resale","excise tax","company tax","insurance")
collegemeanctable <- cbind(collegemeanc, ENCfactor2, sexgrouplc)
collegeENC <- ggplot(as.data.frame(collegemeanctable), aes(factor(ENCfactor2), as.numeric(collegemeanc), fill = as.factor(sexgrouplc))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("People with \n college degree", "people without \n college degree"), 
                    values=c("dodgerblue", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("purchase", "fuel/recharging", "maintenance", "insurance", "excise tax", "resale","company tax")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#driver's licence & factors encouraging buying
licencemeanc <- c(mean(EVINTEREST$ENCpurchase[EVINTEREST$licence==1]),
                  mean(EVINTEREST$ENCmaintenance[EVINTEREST$licence==1]), 
                  mean(EVINTEREST$ENCfuel[EVINTEREST$licence==1]),
                  mean(EVINTEREST$ENCresale[EVINTEREST$licence==1]), 
                  mean(EVINTEREST$ENCextax[EVINTEREST$licence==1]),
                  mean(EVINTEREST$ENCcomptac[EVINTEREST$licence==1]), 
                  mean(EVINTEREST$ENCinsurance[EVINTEREST$licence==1]),
                  mean(EVINTEREST$ENCpurchase[EVINTEREST$licence==0]),
                  mean(EVINTEREST$ENCmaintenance[EVINTEREST$licence==0]), 
                  mean(EVINTEREST$ENCfuel[EVINTEREST$licence==0]),
                  mean(EVINTEREST$ENCresale[EVINTEREST$licence==0]), 
                  mean(EVINTEREST$ENCextax[EVINTEREST$licence==0]),
                  mean(EVINTEREST$ENCcomptac[EVINTEREST$licence==0]), 
                  mean(EVINTEREST$ENCinsurance[EVINTEREST$licence==0]))
licencemeanctable <- cbind(licencemeanc, ENCfactor2, sexgrouplc)
licenceENC <- ggplot(as.data.frame(licencemeanctable), aes(factor(ENCfactor2), as.numeric(licencemeanc), fill = as.factor(sexgrouplc))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("People with \n driver's licence", "people without \n driver's licence"), 
                    values=c("dodgerblue", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("purchase", "fuel/recharging", "maintenance", "insurance", "excise tax", "resale","company tax")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#number of cars & factors encouraging buying
carnomeanc <- c(mean(EVINTEREST$ENCpurchase[EVINTEREST$NumCar==0]),
                mean(EVINTEREST$ENCmaintenance[EVINTEREST$NumCar==0]), 
                mean(EVINTEREST$ENCfuel[EVINTEREST$NumCar==0]),
                mean(EVINTEREST$ENCresale[EVINTEREST$NumCar==0]), 
                mean(EVINTEREST$ENCextax[EVINTEREST$NumCar==0]),
                mean(EVINTEREST$ENCcomptac[EVINTEREST$NumCar==0]), 
                mean(EVINTEREST$ENCinsurance[EVINTEREST$NumCar==0]),
                mean(EVINTEREST$ENCpurchase[EVINTEREST$NumCar==1]),
                mean(EVINTEREST$ENCmaintenance[EVINTEREST$NumCar==1]), 
                mean(EVINTEREST$ENCfuel[EVINTEREST$NumCar==1]),
                mean(EVINTEREST$ENCresale[EVINTEREST$NumCar==1]), 
                mean(EVINTEREST$ENCextax[EVINTEREST$NumCar==1]),
                mean(EVINTEREST$ENCcomptac[EVINTEREST$NumCar==1]), 
                mean(EVINTEREST$ENCinsurance[EVINTEREST$NumCar==1]),
                mean(EVINTEREST$ENCpurchase[EVINTEREST$NumCar==2]),
                mean(EVINTEREST$ENCmaintenance[EVINTEREST$NumCar==2]), 
                mean(EVINTEREST$ENCfuel[EVINTEREST$NumCar==2]),
                mean(EVINTEREST$ENCresale[EVINTEREST$NumCar==2]), 
                mean(EVINTEREST$ENCextax[EVINTEREST$NumCar==2]),
                mean(EVINTEREST$ENCcomptac[EVINTEREST$NumCar==2]), 
                mean(EVINTEREST$ENCinsurance[EVINTEREST$NumCar==2]),
                mean(EVINTEREST$ENCpurchase[EVINTEREST$NumCar==3]),
                mean(EVINTEREST$ENCmaintenance[EVINTEREST$NumCar==3]), 
                mean(EVINTEREST$ENCfuel[EVINTEREST$NumCar==3]),
                mean(EVINTEREST$ENCresale[EVINTEREST$NumCar==3]), 
                mean(EVINTEREST$ENCextax[EVINTEREST$NumCar==3]),
                mean(EVINTEREST$ENCcomptac[EVINTEREST$NumCar==3]), 
                mean(EVINTEREST$ENCinsurance[EVINTEREST$NumCar==3]))
carnomeanctable <- cbind(carnomeanc, ENCfactor4, incgrouplc)
carnoENC <- ggplot(as.data.frame(carnomeanctable), aes(factor(ENCfactor4), as.numeric(carnomeanc), fill = as.factor(incgrouplc))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("no car", "1 car", "2 cars", "3 cars \n or more"), 
                    values=c("dodgerblue", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("purchase", "fuel/recharging", "maintenance", "insurance", "excise tax", "resale","company tax")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous(" ", labels=NULL)

#in scotland & factors encouraging buying
scotmeanc <- c(mean(EVINTEREST$ENCpurchase[EVINTEREST$Scotland==1]),
               mean(EVINTEREST$ENCmaintenance[EVINTEREST$Scotland==1]), 
               mean(EVINTEREST$ENCfuel[EVINTEREST$Scotland==1]),
               mean(EVINTEREST$ENCresale[EVINTEREST$Scotland==1]), 
               mean(EVINTEREST$ENCextax[EVINTEREST$Scotland==1]),
               mean(EVINTEREST$ENCcomptac[EVINTEREST$Scotland==1]), 
               mean(EVINTEREST$ENCinsurance[EVINTEREST$Scotland==1]),
               mean(EVINTEREST$ENCpurchase[EVINTEREST$Scotland==0]),
               mean(EVINTEREST$ENCmaintenance[EVINTEREST$Scotland==0]), 
               mean(EVINTEREST$ENCfuel[EVINTEREST$Scotland==0]),
               mean(EVINTEREST$ENCresale[EVINTEREST$Scotland==0]), 
               mean(EVINTEREST$ENCextax[EVINTEREST$Scotland==0]),
               mean(EVINTEREST$ENCcomptac[EVINTEREST$Scotland==0]), 
               mean(EVINTEREST$ENCinsurance[EVINTEREST$Scotland==0]))
scotmeanctable <- cbind(scotmeanc, ENCfactor2, sexgrouplc)
scotENC <- ggplot(as.data.frame(scotmeanctable), aes(factor(ENCfactor2), as.numeric(scotmeanc), fill = as.factor(sexgrouplc))) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("people in Scotland", "people elsewhere"), 
                    values=c("dodgerblue", "dodgerblue4")) +
  scale_x_discrete("  ",limits=c("purchase", "fuel/recharging", "maintenance", "insurance", "excise tax", "resale","company tax")) +
  labs(fill="  ") +
  theme(panel.grid =element_blank())+
  scale_y_continuous("   ", labels=NULL)

#put them in one
grid.arrange(ageENC, sexENC, incomeENC, collegeENC, licenceENC,carnoENC,scotENC, ncol=2, nrow=4)


