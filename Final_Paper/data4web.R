possible_wd <- c("E:\\学习相关\\R\\Collaborative-Data-Analysis-Assignment2\\Final_Paper", "C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Final_Paper")
repmis::set_valid_wd(possible_wd)

source("data_cleanup.R")

Region <- c('NorthEast','NorthWest','YorkshireHumber','EastMidlands', 'WestMidrands',
            'EastofEngland','London','SouthEast','SouthWest','Wales', 'Scotland')
IntRegion <- c(mean(EVINTEREST$EVinterest[EVINTEREST$NorthEast==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$NorthWest==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$YorkshireHumber==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$EastMidlands==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$WestMidrands==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$EastofEngland==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$London==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$SouthEast==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$SouthWest==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$Wales==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$Scotland==1]))

females <- subset(EVINTEREST, Male==0)
IntFemales <- c(mean(females$EVinterest[females$NorthEast==1]),
               mean(females$EVinterest[females$NorthWest==1]),
               mean(females$EVinterest[females$YorkshireHumber==1]),
               mean(females$EVinterest[females$EastMidlands==1]),
               mean(females$EVinterest[females$WestMidrands==1]),
               mean(females$EVinterest[females$EastofEngland==1]),
               mean(females$EVinterest[females$London==1]),
               mean(females$EVinterest[females$SouthEast==1]),
               mean(females$EVinterest[females$SouthWest==1]),
               mean(females$EVinterest[females$Wales==1]),
               mean(females$EVinterest[females$Scotland==1]))

males <- subset(EVINTEREST, Male==1)
IntMales <- c(mean(males$EVinterest[males$NorthEast==1]),
                mean(males$EVinterest[males$NorthWest==1]),
                mean(males$EVinterest[males$YorkshireHumber==1]),
                mean(males$EVinterest[males$EastMidlands==1]),
                mean(males$EVinterest[males$WestMidrands==1]),
                mean(males$EVinterest[males$EastofEngland==1]),
                mean(males$EVinterest[males$London==1]),
                mean(males$EVinterest[males$SouthEast==1]),
                mean(males$EVinterest[males$SouthWest==1]),
                mean(males$EVinterest[males$Wales==1]),
                mean(males$EVinterest[males$Scotland==1]))

young <- subset(EVINTEREST, agegroup==1)
IntYoung <- c(mean(young$EVinterest[young$NorthEast==1]),
              mean(young$EVinterest[young$NorthWest==1]),
              mean(young$EVinterest[young$YorkshireHumber==1]),
              mean(young$EVinterest[young$EastMidlands==1]),
              mean(young$EVinterest[young$WestMidrands==1]),
              mean(young$EVinterest[young$EastofEngland==1]),
              mean(young$EVinterest[young$London==1]),
              mean(young$EVinterest[young$SouthEast==1]),
              mean(young$EVinterest[young$SouthWest==1]),
              mean(young$EVinterest[young$Wales==1]),
              mean(young$EVinterest[young$Scotland==1]))

middleage <- subset(EVINTEREST, agegroup==2)
IntMiddleage <- c(mean(middleage$EVinterest[middleage$NorthEast==1]),
              mean(middleage$EVinterest[middleage$NorthWest==1]),
              mean(middleage$EVinterest[middleage$YorkshireHumber==1]),
              mean(middleage$EVinterest[middleage$EastMidlands==1]),
              mean(middleage$EVinterest[middleage$WestMidrands==1]),
              mean(middleage$EVinterest[middleage$EastofEngland==1]),
              mean(middleage$EVinterest[middleage$London==1]),
              mean(middleage$EVinterest[middleage$SouthEast==1]),
              mean(middleage$EVinterest[middleage$SouthWest==1]),
              mean(middleage$EVinterest[middleage$Wales==1]),
              mean(middleage$EVinterest[middleage$Scotland==1]))

oldage <- subset(EVINTEREST, agegroup==3)
IntOldage <- c(mean(oldage$EVinterest[oldage$NorthEast==1]),
              mean(oldage$EVinterest[oldage$NorthWest==1]),
              mean(oldage$EVinterest[oldage$YorkshireHumber==1]),
              mean(oldage$EVinterest[oldage$EastMidlands==1]),
              mean(oldage$EVinterest[oldage$WestMidrands==1]),
              mean(oldage$EVinterest[oldage$EastofEngland==1]),
              mean(oldage$EVinterest[oldage$London==1]),
              mean(oldage$EVinterest[oldage$SouthEast==1]),
              mean(oldage$EVinterest[oldage$SouthWest==1]),
              mean(oldage$EVinterest[oldage$Wales==1]),
              mean(oldage$EVinterest[oldage$Scotland==1]))

lowincome <- subset(EVINTEREST, low==1)
IntLow <- c(mean(lowincome$EVinterest[lowincome$NorthEast==1]),
              mean(lowincome$EVinterest[lowincome$NorthWest==1]),
              mean(lowincome$EVinterest[lowincome$YorkshireHumber==1]),
              mean(lowincome$EVinterest[lowincome$EastMidlands==1]),
              mean(lowincome$EVinterest[lowincome$WestMidrands==1]),
              mean(lowincome$EVinterest[lowincome$EastofEngland==1]),
              mean(lowincome$EVinterest[lowincome$London==1]),
              mean(lowincome$EVinterest[lowincome$SouthEast==1]),
              mean(lowincome$EVinterest[lowincome$SouthWest==1]),
              mean(lowincome$EVinterest[lowincome$Wales==1]),
              mean(lowincome$EVinterest[lowincome$Scotland==1]))

lowmiddle <- subset(EVINTEREST, lowermiddle==1)
IntLowmid <- c(mean(lowmiddle$EVinterest[lowmiddle$NorthEast==1]),
              mean(lowmiddle$EVinterest[lowmiddle$NorthWest==1]),
              mean(lowmiddle$EVinterest[lowmiddle$YorkshireHumber==1]),
              mean(lowmiddle$EVinterest[lowmiddle$EastMidlands==1]),
              mean(lowmiddle$EVinterest[lowmiddle$WestMidrands==1]),
              mean(lowmiddle$EVinterest[lowmiddle$EastofEngland==1]),
              mean(lowmiddle$EVinterest[lowmiddle$London==1]),
              mean(lowmiddle$EVinterest[lowmiddle$SouthEast==1]),
              mean(lowmiddle$EVinterest[lowmiddle$SouthWest==1]),
              mean(lowmiddle$EVinterest[lowmiddle$Wales==1]),
              mean(lowmiddle$EVinterest[lowmiddle$Scotland==1]))

highmiddle <- subset(EVINTEREST, highermiddle==1)
IntHighmid <- c(mean(highmiddle$EVinterest[highmiddle$NorthEast==1]),
              mean(highmiddle$EVinterest[highmiddle$NorthWest==1]),
              mean(highmiddle$EVinterest[highmiddle$YorkshireHumber==1]),
              mean(highmiddle$EVinterest[highmiddle$EastMidlands==1]),
              mean(highmiddle$EVinterest[highmiddle$WestMidrands==1]),
              mean(highmiddle$EVinterest[highmiddle$EastofEngland==1]),
              mean(highmiddle$EVinterest[highmiddle$London==1]),
              mean(highmiddle$EVinterest[highmiddle$SouthEast==1]),
              mean(highmiddle$EVinterest[highmiddle$SouthWest==1]),
              mean(highmiddle$EVinterest[highmiddle$Wales==1]),
              mean(highmiddle$EVinterest[highmiddle$Scotland==1]))

highincome <- subset(EVINTEREST, high==1)
IntHigh <- c(mean(highincome$EVinterest[highincome$NorthEast==1]),
  mean(highincome$EVinterest[highincome$NorthWest==1]),
  mean(highincome$EVinterest[highincome$YorkshireHumber==1]),
  mean(highincome$EVinterest[highincome$EastMidlands==1]),
  mean(highincome$EVinterest[highincome$WestMidrands==1]),
  mean(highincome$EVinterest[highincome$EastofEngland==1]),
  mean(highincome$EVinterest[highincome$London==1]),
  mean(highincome$EVinterest[highincome$SouthEast==1]),
  mean(highincome$EVinterest[highincome$SouthWest==1]),
  mean(highincome$EVinterest[highincome$Wales==1]),
  mean(highincome$EVinterest[highincome$Scotland==1]))

collegegraduate <- subset(EVINTEREST, degree==1)
IntCollege <- c(mean(collegegraduate$EVinterest[collegegraduate$NorthEast==1]),
  mean(collegegraduate$EVinterest[collegegraduate$NorthWest==1]),
  mean(collegegraduate$EVinterest[collegegraduate$YorkshireHumber==1]),
  mean(collegegraduate$EVinterest[collegegraduate$EastMidlands==1]),
  mean(collegegraduate$EVinterest[collegegraduate$WestMidrands==1]),
  mean(collegegraduate$EVinterest[collegegraduate$EastofEngland==1]),
  mean(collegegraduate$EVinterest[collegegraduate$London==1]),
  mean(collegegraduate$EVinterest[collegegraduate$SouthEast==1]),
  mean(collegegraduate$EVinterest[collegegraduate$SouthWest==1]),
  mean(collegegraduate$EVinterest[collegegraduate$Wales==1]),
  mean(collegegraduate$EVinterest[collegegraduate$Scotland==1]))

nocollege <- subset(EVINTEREST, degree==0)
IntNocollege <- c(mean(nocollege$EVinterest[nocollege$NorthEast==1]),
  mean(nocollege$EVinterest[nocollege$NorthWest==1]),
  mean(nocollege$EVinterest[nocollege$YorkshireHumber==1]),
  mean(nocollege$EVinterest[nocollege$EastMidlands==1]),
  mean(nocollege$EVinterest[nocollege$WestMidrands==1]),
  mean(nocollege$EVinterest[nocollege$EastofEngland==1]),
  mean(nocollege$EVinterest[nocollege$London==1]),
  mean(nocollege$EVinterest[nocollege$SouthEast==1]),
  mean(nocollege$EVinterest[nocollege$SouthWest==1]),
  mean(nocollege$EVinterest[nocollege$Wales==1]),
  mean(nocollege$EVinterest[nocollege$Scotland==1]))

licence <- subset(EVINTEREST, licence==1)
IntLicence <- c(mean(licence$EVinterest[licence$NorthEast==1]),
  mean(licence$EVinterest[licence$NorthWest==1]),
  mean(licence$EVinterest[licence$YorkshireHumber==1]),
  mean(licence$EVinterest[licence$EastMidlands==1]),
  mean(licence$EVinterest[licence$WestMidrands==1]),
  mean(licence$EVinterest[licence$EastofEngland==1]),
  mean(licence$EVinterest[licence$London==1]),
  mean(licence$EVinterest[licence$SouthEast==1]),
  mean(licence$EVinterest[licence$SouthWest==1]),
  mean(licence$EVinterest[licence$Wales==1]),
  mean(licence$EVinterest[licence$Scotland==1]))

nolicence <- subset(EVINTEREST, licence==0)
IntNolicence <- c(mean(nolicence $EVinterest[nolicence $NorthEast==1]),
  mean(nolicence $EVinterest[nolicence $NorthWest==1]),
  mean(nolicence $EVinterest[nolicence $YorkshireHumber==1]),
  mean(nolicence $EVinterest[nolicence $EastMidlands==1]),
  mean(nolicence $EVinterest[nolicence $WestMidrands==1]),
  mean(nolicence $EVinterest[nolicence $EastofEngland==1]),
  mean(nolicence $EVinterest[nolicence $London==1]),
  mean(nolicence $EVinterest[nolicence $SouthEast==1]),
  mean(nolicence $EVinterest[nolicence $SouthWest==1]),
  mean(nolicence $EVinterest[nolicence $Wales==1]),
  mean(nolicence $EVinterest[nolicence $Scotland==1]))

nocar <- subset(EVINTEREST, NumCar==0)
IntNocar <- c(mean(nocar$EVinterest[nocar$NorthEast==1]),
  mean(nocar$EVinterest[nocar$NorthWest==1]),
  mean(nocar$EVinterest[nocar$YorkshireHumber==1]),
  mean(nocar$EVinterest[nocar$EastMidlands==1]),
  mean(nocar$EVinterest[nocar$WestMidrands==1]),
  mean(nocar$EVinterest[nocar$EastofEngland==1]),
  mean(nocar$EVinterest[nocar$London==1]),
  mean(nocar$EVinterest[nocar$SouthEast==1]),
  mean(nocar$EVinterest[nocar$SouthWest==1]),
  mean(nocar$EVinterest[nocar$Wales==1]),
  mean(nocar$EVinterest[nocar$Scotland==1]))

onecar <- subset(EVINTEREST, NumCar==1)
IntOnecar <- c(mean(onecar$EVinterest[onecar$NorthEast==1]),
  mean(onecar$EVinterest[onecar$NorthWest==1]),
  mean(onecar$EVinterest[onecar$YorkshireHumber==1]),
  mean(onecar$EVinterest[onecar$EastMidlands==1]),
  mean(onecar$EVinterest[onecar$WestMidrands==1]),
  mean(onecar$EVinterest[onecar$EastofEngland==1]),
  mean(onecar$EVinterest[onecar$London==1]),
  mean(onecar$EVinterest[onecar$SouthEast==1]),
  mean(onecar$EVinterest[onecar$SouthWest==1]),
  mean(onecar$EVinterest[onecar$Wales==1]),
  mean(onecar$EVinterest[onecar$Scotland==1]))

twocars <- subset(EVINTEREST, NumCar==2)
IntTwocars <- c(mean(twocars$EVinterest[twocars$NorthEast==1]),
  mean(twocars$EVinterest[twocars$NorthWest==1]),
  mean(twocars$EVinterest[twocars$YorkshireHumber==1]),
  mean(twocars$EVinterest[twocars$EastMidlands==1]),
  mean(twocars$EVinterest[twocars$WestMidrands==1]),
  mean(twocars$EVinterest[twocars$EastofEngland==1]),
  mean(twocars$EVinterest[twocars$London==1]),
  mean(twocars$EVinterest[twocars$SouthEast==1]),
  mean(twocars$EVinterest[twocars$SouthWest==1]),
  mean(twocars$EVinterest[twocars$Wales==1]),
  mean(twocars$EVinterest[twocars$Scotland==1]))

threecars <- subset(EVINTEREST, NumCar==3)
IntThreecars <- c(mean(threecars$EVinterest[threecars$NorthEast==1]),
  mean(threecars$EVinterest[threecars$NorthWest==1]),
  mean(threecars$EVinterest[threecars$YorkshireHumber==1]),
  mean(threecars$EVinterest[threecars$EastMidlands==1]),
  mean(threecars$EVinterest[threecars$WestMidrands==1]),
  mean(threecars$EVinterest[threecars$EastofEngland==1]),
  mean(threecars$EVinterest[threecars$London==1]),
  mean(threecars$EVinterest[threecars$SouthEast==1]),
  mean(threecars$EVinterest[threecars$SouthWest==1]),
  mean(threecars$EVinterest[threecars$Wales==1]),
  mean(threecars$EVinterest[threecars$Scotland==1]))

RegionDF <- data.frame(OID=c(1,2,3,4,5,6,7,8,9,10,11),
  Region=Region, Interest=IntRegion, IntFemales, IntMales, 
  IntYoung, IntMiddleage, IntOldage, IntLow, IntLowmid, IntHighmid, IntHigh,
  IntCollege, IntNocollege, IntLicence, IntNolicence,
  IntNocar, IntOnecar, IntTwocars, IntThreecars)

save(RegionDF, file="data4web.rda")


