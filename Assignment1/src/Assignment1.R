install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
install.packages("hrbrthemes")
install.packages("ggridges")
library(ggridges)
library(ggplot2)
library(hrbrthemes)
install.packages("tidyverse")
install.packages("geojsonio")
install.packages("RColorBrewer")
install.packages("sp")
library(sp)
install.packages("rgdal")
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
install.packages("broom")
library(broom)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)


TXCase <- read.csv("COVID-19_cases_TX.csv")

summary(TXCase)

which(TXCase$confirmed_cases, confirmed_cases>0)

#======================================================================================================
# 1: When did the first case happen in each county in Texas
## 1.1: Table with counties and first case date
current <- "null"
linen = 1
FirstCase = data.frame(county_fips_code = numeric(), county_name = character(), state = character(), state_fips_code = numeric(), date = character(), confirmed_cases = numeric(), deaths = numeric())
for(i in 1:nrow(TXCase)){
  if(TXCase$county_name[i] != current && TXCase$confirmed_cases[i] > 0){
    FirstCase <- rbind(FirstCase,TXCase[i,] )
    current = TXCase$county_name[i]
    linen = linen + 1
  }
}
FirstCase <- arrange(FirstCase,date,county_name)

print(FirstCase[1:20,])
summary(FirstCase)

## 1.2: Graph with number of counties that show first case by date
FirstCaseByDate <- ddply(FirstCase,.(date),summarize,number=length(date))
FirstCaseByDate$date <- as.Date(FirstCaseByDate$date,"%Y/%m/%d")
FirstCaseByDate <- arrange(FirstCaseByDate,date)
total = 0
for(i in 1:nrow(FirstCaseByDate)){
  total = total + FirstCaseByDate$number[i]
  FirstCaseByDate$number[i] = total
}
Sys.setlocale("LC_TIME", "English")

print(FirstCaseByDate[1:20,])
summary(FirstCaseByDate)
ggplot(FirstCaseByDate, aes(x=date, y=number)) + geom_line( color="#69b3a2", size=2, alpha=0.9) + ggtitle("Number of counties that show first case by date in 2020")





#======================================================================================================
# 2: How fast did the virus spread in Texas
## 2.1:  Find the avg number of cases in each county with the top 20
SpreadRate = data.frame(county_fips_code = character(), county_name = character(), confirmed_cases = numeric(), ndate =  numeric(), rate = numeric())

current <- "null"
linen = 0
for(i in 1:nrow(TXCase)){
  if(current != TXCase$county_name[i]){
    SpreadRate <- rbind(SpreadRate, data.frame(county_fips_code = as.character(linen+1), county_name = TXCase$county_name[i], confirmed_cases = 1, ndate =  1, rate = 0))                         
    current = TXCase$county_name[i]
    linen = linen + 1
    if(linen != 1){
      SpreadRate[linen-1,"rate"] <-  (SpreadRate[linen-1,"confirmed_cases"] /  SpreadRate[linen-1,"ndate"])
    }
  }
  else{
    SpreadRate[linen,"confirmed_cases"] <- SpreadRate[linen,"confirmed_cases"] + TXCase$confirmed_cases[i]
    SpreadRate[linen,"ndate"] <- SpreadRate[linen,"ndate"] + 1
  }
}


SpreadRate[linen,"rate"] <-  (SpreadRate[linen,"confirmed_cases"] /  SpreadRate[linen,"ndate"])
SpreadRate <- SpreadRate[order(SpreadRate$rate,decreasing = T),]

print(SpreadRate[1:20,])
summary(SpreadRate)



## 2.2: Visualize the data on the map. Reference: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
SpreadRatemMap <- geojson_read("TX-48-texas-counties.geojson",  what = "sp")
CensusData <- read.csv("COVID-19_cases_plus_census.csv")
Population <- data.frame(CensusData[,1:8])
Population$county_fips_code <- as.character(Population$county_fips_code - 48000)
Population <- arrange(Population, county_name, geo_id)
for(i in 1: nrow(Population)){
  Population[i,"county_fips_code"] <- as.character(i)
}
Summary(Population)

Population_fortified <- tidy(SpreadRatemMap)
Population_fortified <- Population_fortified %>%
  left_join(. , Population, by=c("id"= "county_fips_code"))
ggplot() +
  geom_polygon(Population = Population_fortified, aes(fill =  Population_fortified$total_pop, x = Population_fortified$long, y = Population_fortified$lat, group = Population_fortified$group)) +
  scale_fill_gradient(trans = "log", low = 'lightskyblue1', high = 'black') +
  theme_void() +
  coord_map() +
  ggtitle("The population density of each county")



SpreadRatemMap_fortified <- tidy(SpreadRatemMap)
SpreadRatemMap_fortified <- SpreadRatemMap_fortified %>%
  left_join(. , SpreadRate, by=c("id"= "county_fips_code"))
ggplot() +
  geom_polygon(SpreadRate = SpreadRatemMap_fortified, aes(fill =  SpreadRatemMap_fortified$rate, x = SpreadRatemMap_fortified$long, y = SpreadRatemMap_fortified$lat, group = SpreadRatemMap_fortified$group)) +
  scale_fill_gradient(trans = "log", low = 'brown1', high = 'black') +
  theme_void() +
  coord_map() +
  ggtitle("The spread rate of each county")






#======================================================================================================
#3. What is the social distancing response and how long did it take after the first case? (need to run section 1 to get the data fram for first case)
##3.1 The relationship between the first date of case and and the date that issuing statewide stay-at-home policy
SocialDistancingDate <- as.Date("2020/04/02", "%Y/%m/%d")
ggplot(FirstCaseByDate, aes(x=date, y=number)) + geom_line( color="#69b3a2", size=2, alpha=0.9)  + geom_vline(xintercept=SocialDistancingDate, color="orange", size=2) +ggtitle("Number of counties that show first case by date in 2020")

##3.2 Does the statewide stay-at-home policy affect the daily new cases?
totalcase = numeric()
total = 0
CasewithDate <- ddply(TXCase,.(date),summarize,casenumber=sum(confirmed_cases))
CasewithDate$date <- as.Date(CasewithDate$date, "%Y/%m/%d")
CasewithDate <- arrange(CasewithDate,date,casenumber)
for(i in 1:370){
  total = total + CasewithDate$casenumber[i]
  totalcase[i] <- total
}
CasewithDate$totalnumber <- totalcase
summary(CasewithDate)

ggplot(CasewithDate, aes(x=date, y=casenumber)) + geom_line( color="red", size=2, alpha=0.9) + theme_ipsum() + geom_vline(xintercept=SocialDistancingDate, color="orange", size=2) +ggtitle("The of Daily Cases in Texas")
ggplot(CasewithDate, aes(x=date, y=totalnumber)) + geom_line( color="blue", size=2, alpha=0.9) + theme_ipsum() + geom_vline(xintercept=SocialDistancingDate, color="orange", size=2) +ggtitle("The Total Cases in Texas")





#======================================================================================================
#4. Does the spread of COVID-19 relate to ages?
CensusData <- read.csv("COVID-19_cases_plus_census.csv")
DatawithAge <- data.frame(c(CensusData[,1:9],CensusData[,34:49]))
oldpercentlist = numeric()
casepercentlist = numeric()
deathpercentlist = numeric()
for(i in 1:254){
  nofold = sum(DatawithAge[i,10:25])
  oldpercentlist[i]  = nofold / DatawithAge[i,"total_pop"] * 100
  casepercentlist[i] = DatawithAge[i,"confirmed_cases"] / DatawithAge[i,"total_pop"] * 100
  deathpercentlist[i] = DatawithAge[i,"deaths"] / DatawithAge[i,"total_pop"] * 100
}

DatawithAge$OldPercent <- oldpercentlist
DatawithAge$CasePercent <- casepercentlist
DatawithAge$DeathPercent <- deathpercentlist
summary(DatawithAge)

#--------------------------------------------------------------------------------
ggplot(DatawithAge, aes(x=median_age, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="blue", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the medium ages")

ggplot(DatawithAge, aes(x=median_age, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the medium age")
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
ggplot(DatawithAge, aes(x=OldPercent, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="blue", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the percentage of people(age60+)")

ggplot(DatawithAge, aes(x=OldPercent, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the percentage of people(age60+)")
#--------------------------------------------------------------------------------



#======================================================================================================
#5. Does the spread of COVID-19 relate to civilians' income?
DatawithIncome <- data.frame(c(CensusData[,1:10], CensusData[,18:33], CensusData[,50:52]))
DatawithIncome$poverty <- CensusData$poverty

wealthypercentlist = numeric()
midclasspercentlist = numeric()
lowincomepercentlist = numeric()
casepercentlist = numeric()
deathpercentlist = numeric()

for(i in 1:254){
  nofpeople = sum(DatawithIncome[i,11:26])
  wealthypercentlist[i] = sum(DatawithIncome[i,23:26]) / nofpeople * 100
  midclasspercentlist[i] = sum(DatawithIncome[i,15:22]) / nofpeople * 100
  lowincomepercentlist[i] = sum(DatawithIncome[i,11:14]) / nofpeople * 100
  casepercentlist[i] = DatawithIncome[i,"confirmed_cases"] / DatawithIncome[i,"total_pop"] * 100
  deathpercentlist[i] = DatawithIncome[i,"deaths"] / DatawithIncome[i,"total_pop"] * 100
}

DatawithIncome$WealthyPercent <- wealthypercentlist
DatawithIncome$MidClassPercent <- midclasspercentlist
DatawithIncome$LowIncomePercent <- lowincomepercentlist
DatawithIncome$CasePercent <-  casepercentlist
DatawithIncome$DeathPercent <- deathpercentlist

summary(DatawithIncome)

#--------------------------------------------------------------------------------
ggplot(DatawithIncome, aes(x=median_income, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="orange", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the average income")

ggplot(DatawithIncome, aes(x=median_income, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="orange", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the average income")
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
ggplot(DatawithIncome, aes(x=LowIncomePercent, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the percentage of people with low income(<25000)")

ggplot(DatawithIncome, aes(x=LowIncomePercent, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the percentage of people with low income(<25000)")
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
ggplot(DatawithIncome, aes(x=MidClassPercent, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="blue", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the percentage of people with meidum-income")

ggplot(DatawithIncome, aes(x=MidClassPercent,, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="blue", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the percentage of people with meidum-income")
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
ggplot(DatawithIncome, aes(x=WealthyPercent, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="green", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the percentage of wealthy people(>100000)")

ggplot(DatawithIncome, aes(x=WealthyPercent,, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="green", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the percentage of of wealthy people(>100000)")
#--------------------------------------------------------------------------------



#======================================================================================================
#6. Does the spread of COVID-19 relate to the educational background? (case select mail 40-65)
DatawithEdu <- data.frame(c(CensusData[,1:8], CensusData[,61:68]))

highedupercentlist = numeric()
midedupercentlist = numeric()
lowedupercentlist = numeric()
casepercentlist = numeric()
deathpercentlist = numeric()

for(i in 1:254){
  highedupercentlist[i] = sum(DatawithEdu[i,9:11]) / DatawithEdu$male_45_to_64[i] * 100
  midedupercentlist[i] = sum(DatawithEdu[i,13:15]) / DatawithEdu$male_45_to_64[i] * 100
  lowedupercentlist[i] = sum(DatawithEdu[i,12]) / DatawithEdu$male_45_to_64[i] * 100
  casepercentlist[i] = DatawithEdu[i,"confirmed_cases"] / DatawithEdu[i,"total_pop"] * 100
  deathpercentlist[i] = DatawithEdu[i,"deaths"] / DatawithEdu[i,"total_pop"] * 100
}

DatawithEdu$LowEduPercent <- lowedupercentlist
DatawithEdu$MidEduPercent <- midedupercentlist
DatawithEdu$HighEduPercent <- highedupercentlist
DatawithEdu$CasePercent <-  casepercentlist
DatawithEdu$DeathPercent <- deathpercentlist

summary(DatawithEdu)

#--------------------------------------------------------------------------------
ggplot(DatawithEdu, aes(x=LowEduPercent, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the percentage of people(40-65) with low educational background")

ggplot(DatawithEdu, aes(x=LowEduPercent, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the percentage of people(40-65) with low educational background")
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
ggplot(DatawithEdu, aes(x=MidEduPercent, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="blue", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the percentage of people(40-65) with medium educational background")

ggplot(DatawithEdu, aes(x=MidEduPercent, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="blue", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the percentage of people(40-65) with medium educational background")
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
ggplot(DatawithEdu, aes(x=highedupercentlist, y=CasePercent)) +
  geom_point() +
  geom_smooth(method=lm , color="green", fill="#69b3a2", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of confirmed cases with the percentage of people(40-65) with high educational background")

ggplot(DatawithEdu, aes(x=highedupercentlist, y=DeathPercent)) +
  geom_point() +
  geom_smooth(method=lm , color="green", fill="gray", size=1.2, se=TRUE) +
  theme_ipsum() +
  ggtitle("The percentage of death cases with the percentage of people(40-65) with high educational background")
#--------------------------------------------------------------------------------







