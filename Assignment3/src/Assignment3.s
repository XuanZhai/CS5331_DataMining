install.packages("DT")
install.packages("seriation")
install.packages("FSelector")
install.packages("caret")
install.packages("doParallel")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(tidyverse)
library(DT)
library(seriation)
library(FSelector)
library(rpart.plot)
library(rpart)
library(ggplot2)
library(lattice)
library(foreach)
library(parallel)
library(caret)
library(doParallel)
library(pROC)
library(pastecs)
cl <- makeCluster(2)
registerDoParallel(cl)


draw_confusion_matrix <- function(cm, titlele) {
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(titlele, cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Endanger', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Not Endanger', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Endanger', cex=1.2, srt=90)
  text(140, 335, 'Not Endanger', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  











# Section 1: Data preparation
CensusData <- read.csv("COVID-19_cases_plus_census.csv")
CensusData <- CensusData %>% mutate_if(is.character, factor)
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))

CensusData <- CensusData %>% filter(confirmed_cases > 0)      # Load the census data


TotalData <- data.frame(
  county_name = CensusData$county_name,
  state = CensusData$state,
  confirmed_cases = CensusData$confirmed_cases,
  deaths = CensusData$deaths,
  cases_percent = CensusData$confirmed_cases / CensusData$total_pop * 100,
  deaths_percent = CensusData$deaths / CensusData$total_pop * 100,
  population_density = CensusData$total_pop / CensusData$area,
  male_pop_density = CensusData$male_pop / CensusData$total_pop * 100,
  public_trans = CensusData$commuters_by_public_transportation,
  old_percent = (CensusData$male_60_61 + CensusData$male_62_64 + CensusData$male_65_to_66 + CensusData$male_67_to_69 +
                   CensusData$male_70_to_74 + CensusData$male_75_to_79 + CensusData$male_80_to_84 + CensusData$male_85_and_over
                 + CensusData$female_60_to_61 + CensusData$female_62_to_64 + CensusData$female_65_to_66 + CensusData$female_67_to_69
                 + CensusData$female_70_to_74 + CensusData$female_75_to_79 + CensusData$female_80_to_84 + CensusData$female_85_and_over)
  / CensusData$total_pop * 100,
  
  umemployed_percent = (CensusData$unemployed_pop + CensusData$not_in_labor_force) / (CensusData$unemployed_pop + CensusData$not_in_labor_force + CensusData$employed_pop),
  Gini_index = CensusData$gini_index,
  Nohigh_degree = 1 - (CensusData$masters_degree + CensusData$graduate_professional_degree + CensusData$bachelors_degree + CensusData$associates_degree)/CensusData$total_pop,       
  
  median_age = CensusData$median_age,
  median_income = CensusData$median_income,
  income_on_rent = CensusData$percent_income_spent_on_rent,
  LongCommTime = (CensusData$commute_less_10_mins + CensusData$commute_10_14_mins + CensusData$commute_15_19_mins + 
                 CensusData$commute_20_24_mins + CensusData$commute_25_29_mins + CensusData$commute_30_34_mins +
                 CensusData$commute_35_44_mins) / (
                 CensusData$commute_less_10_mins + CensusData$commute_10_14_mins + CensusData$commute_15_19_mins + 
                 CensusData$commute_20_24_mins + CensusData$commute_25_29_mins + CensusData$commute_30_34_mins +
                 CensusData$commute_35_44_mins + CensusData$commute_45_59_mins + CensusData$commute_60_more_mins) * 100
)              #Load the data set for preparation






summary(TotalData)
stat.desc(TotalData)



cm <- cor(TotalData %>% select_if(is.numeric ) %>% na.omit)
hmap(cm, margins = c(14,14))                                  # Find the correlation of the total data





# Section 2: Find class
TotalData <- TotalData %>% mutate(danger = as.factor( (cases_percent > 5 & population_density > 200 ) | (cases_percent > 8 & population_density <= 200)))               # Determine the class for danger
TotalData %>% pull(danger) %>% table()


TotalData %>% group_by(state) %>%
  summarize(danger_pct =sum(danger == TRUE)/n()) %>%
  arrange(desc(danger_pct))                                  # Find the list of states with the most amount of dangerous counties.


Data_train <- TotalData %>% filter(state %in% c("MD","CA","MS","DE","TX","NY","IA","KS","NV","ND"))          # choose states for training. (include rural & flourishing states)
Data_train %>% pull(danger) %>% table()

Data_test <- TotalData %>% filter(!state %in% c("MD","CA","MS","DE","TX","NY","IA","KS","NV","ND"))
Data_test %>% pull(danger) %>% table()


counties <- as_tibble(map_data("county"))
counties <- counties %>%
  rename(c(county=subregion, state = region)) %>%
  mutate(state = state.abb[match(state,tolower(state.name))]) %>%
  select(state,county,long,lat,group)


counties_all <- counties %>% left_join( Data_train %>%
                                       mutate(county = county_name %>% str_to_lower() %>%
                                       str_replace('\\s+county\\s*$','')))


ggplot(counties_all, aes(long,lat)) +
  geom_polygon(aes(group = group, fill = danger), color = "black", size = 0.1) +
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))      # Build up the map for training states





# Section 3: Check Importance

Data_train %>% chi.squared(danger ~ ., data = .) %>%
  arrange(desc(attr_importance)) %>% head()


Data_train_c <- Data_train %>% select(-c(cases_percent, county_name, deaths_percent, confirmed_cases))
Data_train_c %>% chi.squared(danger ~ ., data = .) %>%
  arrange(desc(attr_importance)) %>% head()




# Section 4.1: rpart method
fit_rpart <- Data_train_c %>%
  train(danger ~ . - state,
        data = . ,
        method = "rpart",
        control = rpart.control(minsplit = 2),
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 5
  )


rpart.plot(fit_rpart$finalModel, extra = 2)
fit_rpart
ggplot(fit_rpart)
varImp(fit_rpart)

Data_test <- Data_test %>% na.omit
Data_test_rpart <- Data_test
Data_test_rpart$danger_predicted <- predict(fit_rpart, Data_test)

counties_test <- counties %>% left_join(Data_test_rpart %>% 
                         mutate(county = county_name %>% str_to_lower() %>% 
                         str_replace('\\s+county\\s*$', '')))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'green'))

cm1 <- confusionMatrix(data = Data_test_rpart$danger_predicted, ref = Data_test_rpart$danger)
draw_confusion_matrix(cm1, "Confusion Matrix for rpart method")


# Section 4.2: knn method
train_index <- createFolds(Data_train_c$danger, k = 10)

fit_knn <- Data_train_c %>% train(
   danger ~ . - state,
   data = .,
   method = "knn",
   preProcess = "scale",
   tuneLength = 5,
   trControl = trainControl(method = "cv", indexOut = train_index)
)

Data_test_knn <- Data_test
Data_test_knn$danger_predicted <- predict(fit_knn, Data_test)

counties_test_knn <- counties %>% left_join(Data_test_knn %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))

ggplot(counties_test_knn, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))

ggplot(counties_test_knn, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'green'))



cm2 <- confusionMatrix(data = Data_test_knn$danger_predicted, ref = Data_test_knn$danger)
draw_confusion_matrix(cm2, "Confusion Matrix for knn method")



# Section 4.3: rpart method with cost 
cost <- matrix(c(
  0,5,
  1, 0
), byrow = TRUE, nrow = 2)



fit_rpart_c <- Data_train_c %>%
  train(danger ~ . - state,
        data = . ,
        method = "rpart",
        parms = list(loss = cost),
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 5
  )

fit_rpart_c
varImp(fit_rpart_c)
rpart.plot(fit_rpart_c$finalModel, extra = 2)

Data_test_rpart_c <- Data_test
Data_test_rpart_c$danger_predicted <- predict(fit_rpart_c, Data_test)

counties_test_rpart_c <- counties %>% left_join(Data_test_knn %>% 
                     mutate(county = county_name %>% str_to_lower() %>% 
                     str_replace('\\s+county\\s*$', '')))

ggplot(counties_test_rpart_c, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))

ggplot(counties_test_rpart_c, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'green'))


cm3 <- confusionMatrix(data = Data_test_rpart_c$danger_predicted, ref = Data_test_rpart_c$danger)
draw_confusion_matrix(cm3, "Confusion Matrix for rpart method with cost")




# Section 4.4: Conditional Inference Tree

fit_ctree <- Data_train_c %>% 
  train(danger ~ . - state,
        method = "ctree",
        data = .,
        tuneLength = 5,
        trControl = trainControl(method = "cv", indexOut = train_index))
fit_ctree

plot(fit_ctree$finalModel)


Data_test_ctree <- Data_test
Data_test_ctree$danger_predicted <- predict(fit_ctree, Data_test)


counties_test_ctree <- counties %>% left_join(Data_test_ctree %>% 
                                                  mutate(county = county_name %>% str_to_lower() %>% 
                                                           str_replace('\\s+county\\s*$', '')))

ggplot(counties_test_ctree, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))

ggplot(counties_test_ctree, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'green'))


cm4 <- confusionMatrix(data = Data_test_ctree$danger_predicted, ref = Data_test_ctree$danger)
draw_confusion_matrix(cm4, "Confusion Matrix for Conditional Inference Tree method")


# Section 5: Comparison

compare_method <- resamples(list(
  CART = fit_rpart,
  kNearestNeighbors = fit_knn,
  CART_Cost = fit_rpart_c,
  Ctree = fit_ctree
))
summary(compare_method)

difs <- diff(compare_method)
difs
summary(difs)




# Section 6: State prediction

Case_Data <- read.csv("covid_confirmed_usafacts.csv")

Case_Data <- subset(Case_Data, select = -c(countyFIPS,County.Name, StateFIPS) )

Case_Data <- Case_Data %>% 
  group_by(state) %>% 
  summarise(across(everything(), sum))



for(i in 1:51){
  Case_Data$rate[i] = (Case_Data$X2021.4.28[i] - Case_Data$X2021.1.12[i])/107
}


Case_Data <- subset(Case_Data, select = c(state,rate))
colnames(Case_Data)[2] <- "spread_rate"


Vac_Data <- read.csv("us_state_vaccinations.csv")

Vac_Data$tvac_rate <- (Vac_Data$total_vaccinations_E - Vac_Data$total_vaccinations_S) / 107
Vac_Data$pvac_rate <- (Vac_Data$people_vaccinated_per_hundred_E - Vac_Data$people_vaccinated_per_hundred_S) / 107
Vac_Data <- subset(Vac_Data, select = c(location, state, people_vaccinated_per_hundred_E, tvac_rate, pvac_rate) )
colnames(Vac_Data)[3] <- "people_vaccinated_per_hundred"


TotalData2 <- data.frame(
  state = CensusData$state,
  cases_percent = CensusData$confirmed_cases / CensusData$total_pop * 100,
  deaths_percent = CensusData$deaths / CensusData$total_pop * 100,
  population_density = CensusData$total_pop / CensusData$area,
  Gini_index = CensusData$gini_index,
  Nohigh_degree = 1 - (CensusData$masters_degree + CensusData$graduate_professional_degree + CensusData$bachelors_degree + CensusData$associates_degree)/CensusData$total_pop,       
  median_age = CensusData$median_age,
  median_income = CensusData$median_income
)


TotalData2 <- TotalData2 %>% 
  group_by(state) %>% 
  summarise(cases_percent = mean(cases_percent), deaths_percent = mean(deaths_percent), population_density = mean(population_density),
            Gini_index = mean(Gini_index), Nohigh_degree = mean(Nohigh_degree), median_income = mean(median_income), median_age = mean(median_age))

TotalData2 <- merge(TotalData2, Vac_Data, by= "state")
TotalData2 <- merge(TotalData2, Case_Data, by= "state")


summary(TotalData2)
stat.desc(TotalData2)


cm <- cor(TotalData2 %>% select_if(is.numeric ) %>% na.omit)
hmap(cm, margins = c(14,14))




# Section 6.1: Find Class for part 2
TotalData2 <- TotalData2 %>% mutate(danger = as.factor(spread_rate > 2000 | deaths_percent > 0.15))
TotalData2 %>% pull(danger) %>% table()


TotalData2 %>% group_by(state) %>%
  summarize(danger_pct =sum(danger == TRUE)/n()) %>%
  arrange(danger_pct)




Data_train_2 <- TotalData2 %>% filter(state %in% c("MD","CA","MS","DE","TX","NY","IA","KS","NV","ND"))
Data_train_2 %>% pull(danger) %>% table()

Data_test_2 <- TotalData2 %>% filter(!state %in% c("MD","CA","MS","DE","TX","NY","IA","KS","NV","ND"))
Data_test_2 %>% pull(danger) %>% table()


states <- as_tibble(map_data("state"))
states <- states %>%
  rename(state = region) 


states_all <- states %>% left_join( Data_train_2 %>%
                                          mutate(state = location %>% str_to_lower()))



ggplot(states_all, aes(long,lat)) +
  geom_polygon(aes(group = group, fill = danger), color = "black", size = 0.1) +
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))




# Section 6.2: Check importance for part 2

Data_train_2 %>% chi.squared(danger ~ ., data = .) %>%
  arrange(desc(attr_importance)) %>% head()


Data_train_2 <- Data_train_2 %>% select(-c(location,state, spread_rate, deaths_percent, cases_percent))
Data_train_2 %>% chi.squared(danger ~ ., data = .) %>%
  arrange(attr_importance) %>% head()




# Section 6.3: rpart method for part 2
fit_rpart_2 <- Data_train_2 %>%
  train(danger ~ .,
        data = . ,
        method = "rpart",
        control = rpart.control(minsplit = 2),
        
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 5
  )


rpart.plot(fit_rpart_2$finalModel, extra = 2)



Data_test_2 <- Data_test_2 %>% na.omit
Data_test_rpart_2 <- subset(Data_test_2, select = -c(location))


Data_test_2$danger_predicted <- predict(fit_rpart_2, Data_test_rpart_2)

state_test_2 <- states %>% left_join( Data_test_2 %>%
                                        mutate(state = location %>% str_to_lower()))


ggplot(state_test_2, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'blue'))

ggplot(state_test_2, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = danger_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'green'))

cm5<-confusionMatrix(data = Data_test_2$danger_predicted, ref = Data_test_2$danger)

draw_confusion_matrix(cm5, "Confusion Matrix for rpart method for section 2")


