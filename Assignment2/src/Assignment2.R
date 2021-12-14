install.packages("tidyverse")
install.packages("dbscan")
install.packages("cluster")
install.packages("seriation")
install.packages("mclust")
install.packages("ggplot2")
install.packages("tibble")
install.packages("maps")
install.packages("ggmap")
install.packages("pastecs")
install.packages("corrplot")
library(corrplot)
library(pastecs)
library(mclust)
library(dbscan)
library(tidyverse)
library(cluster)
library(seriation)
library(ggplot2)
library(tibble)
library(maps)
library(ggmap)

CensusData <- read.csv("COVID-19_cases_plus_census.csv")
PopDensity <- CensusData$total_pop / as.numeric(CensusData$area) # Total population / the size of a county in km^2
CaseRate <- CensusData$confirmed_cases / CensusData$total_pop   # Confirmed cases / total population
DeathRate <- CensusData$deaths / CensusData$total_pop     # Death cases / total population
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))


#====================== Part 1 =================================================
# Cluster data about the income and educational background of people in each county

IncAndEdu <- data.frame(c(CensusData[,1:11],CensusData[,24:39], CensusData[,67:74]))

highedupercentlist = numeric()
midedupercentlist = numeric()
lowedupercentlist = numeric()

for(i in 1:254){
  highedupercentlist[i] = sum(IncAndEdu[i,32:34]) / IncAndEdu$male_45_to_64[i]
  midedupercentlist[i] = sum(IncAndEdu[i,29:31]) / IncAndEdu$male_45_to_64[i]
  lowedupercentlist[i] = sum(IncAndEdu[i,28]) / IncAndEdu$male_45_to_64[i]
}

IncAndEdu$LowEduPercent <- lowedupercentlist
IncAndEdu$MidEduPercent <- midedupercentlist
IncAndEdu$HighEduPercent <- highedupercentlist

summary(IncAndEdu)

IncEduDensity <- data.frame(CensusData$median_age, IncAndEdu$median_income, IncAndEdu$LowEduPercent, IncAndEdu$MidEduPercent, IncAndEdu$HighEduPercent, PopDensity, CaseRate,DeathRate)

colnames(IncEduDensity)[1] <- "MeidianAge"
colnames(IncEduDensity)[2] <- "MeidianIncome"
colnames(IncEduDensity)[3] <- "LowEduPerc"
colnames(IncEduDensity)[4] <- "MidEduPerc"
colnames(IncEduDensity)[5] <- "HighEduPerc"

NIncEduDensity <- IncEduDensity%>% scale() %>% as_tibble()
km4 <- kmeans(NIncEduDensity, centers = 4)    # Use k-means for clustering   # can be modified
ggplot(pivot_longer(as_tibble(km4$centers,  rownames = "cluster"), #visualize the result in a clustering profile.
  cols = colnames(km4$centers)), 
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) + ggtitle("Cluster Profiles") +theme(plot.title = element_text(hjust = 0.5))          


Nkm4 <- IncEduDensity %>% add_column(cluster = factor(km4$cluster)) # The average of the percentage of confirmed cases and deaths 
Nkm4 %>% group_by(cluster) %>% summarize(
cases_percent = mean(CaseRate), 
deaths_percent = mean(DeathRate))


IncEduDensity$county_name <- CensusData$county_name
counties <- as_tibble(ggplot2::map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))
IncEduDensity <- IncEduDensity %>% mutate(county = county_name %>% 
                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

NIncEduDensity_clust <- counties_TX %>% left_join(IncEduDensity %>% 
                                                 add_column(cluster = factor(Nkm4$cluster)))

ggplot(NIncEduDensity_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Visualize clusters on the map of Texas") +theme(plot.title = element_text(hjust = 0.5))

corrplot(cor(IncEduDensity),
         method = "number",
         type = "upper" # show only upper side
)


#====================== Part 2 =================================================
#Clustering analysis relates to commute and public transportation
CommInc <- data.frame(c(CensusData[,1:39],CensusData[,79:83]))

AvgTtoWork <- numeric()
LongCommutelist <- numeric()
PublicCommutelist <- numeric()

for(i in 1:254){
  LongCommutelist[i] = sum(CommInc[i,17:22])/sum(CommInc[i,12:22])
  AvgTtoWork[i] = CommInc$aggregate_travel_time_to_work[i] / CommInc$total_pop[i]
  PublicCommutelist[i] = (CommInc$commuters_by_bus[i] + CommInc$commuters_by_carpool[i] + CommInc$commuters_by_subway_or_elevated[i]) / (CommInc$commuters_by_bus[i] + CommInc$commuters_by_car_truck_van[i] + CommInc$commuters_by_subway_or_elevated[i])
}

CommInc$AvgTtoWork <- AvgTtoWork
CommInc$LongCommutelist <- LongCommutelist
CommInc$PublicCommutelist <- PublicCommutelist





#---------------------- Part 2.1 -----------------------------------------------
Income_Time <- data.frame(CommInc$median_income, CommInc$AvgTtoWork)

colnames(Income_Time)[1] <- "Median_Income"     # Use the Hierarchical Clustering
colnames(Income_Time)[2] <- "AvgTimetoWork"
Income_Time_scaled <- Income_Time %>% scale_numeric()
dist1 <- dist(Income_Time_scaled)
hc1 <- hclust(dist1, method = "average")
plot(hc1)

Income_Time_cluster <- cutree(hc1, k = 2)
Income_Time_cluster_complete <- Income_Time_scaled %>%
  add_column(cluster = factor(Income_Time_cluster))
                                              # Similarity between median income and the average time that travel to work
ggplot(Income_Time_cluster_complete, aes(Median_Income, AvgTimetoWork, color = cluster)) +
  geom_point()  + ggtitle("People's Median Income and their average time travel to work (HC)") +theme(plot.title = element_text(hjust = 0.5))


pcc2 <- pam(dist1, k = 3)
Income_Time_cluster_2 <- Income_Time_scaled %>% add_column(cluster = factor(pcc2$cluster))

medoids3 <- as_tibble(Income_Time_scaled[pcc2$medoids, ], rownames = "cluster")

ggplot(Income_Time_cluster_2, aes(x = Median_Income, y = AvgTimetoWork, color = cluster)) + geom_point()+
  geom_point(data = medoids3, aes(x = Median_Income, y = AvgTimetoWork, color = cluster), shape = 3, size = 10) + ggtitle("People's Median Income and their average time travel to work (PAM)") +theme(plot.title = element_text(hjust = 0.5))# Graph with clustering



#---------------------- Part 2.2 -----------------------------------------------

Public_Density <- data.frame(PopDensity, CommInc$PublicCommutelist)
colnames(Public_Density)[1] <- "Population_Density"
colnames(Public_Density)[2] <- "Public_Commute_Percent"

ggplot(Public_Density, aes(Population_Density, Public_Commute_Percent)) +
  geom_point() + ggtitle("Population density and percentage of public transportation (No cleaning)") +theme(plot.title = element_text(hjust = 0.5))


Public_Density_scaled <- Public_Density[Public_Density$Population_Density < 30,] %>% scale_numeric()
dist <- dist(Public_Density_scaled)       # Similarity between population density and the percentage of civilians who take public transportation
hc <- hclust(dist, method = "complete")
plot(hc)
pimage(dist,order=order(hc))


Public_Density_cluster <- cutree(hc, k = 3)
Public_Density_cluster_complete <- Public_Density_scaled %>%
  add_column(cluster = factor(Public_Density_cluster))

ggplot(Public_Density_cluster_complete, aes(Population_Density, Public_Commute_Percent, color = cluster)) +
  geom_point() + ggtitle("Population Density and percentage of public transportation (HC)") +theme(plot.title = element_text(hjust = 0.5))
  

pcc3 <- pam(dist, k = 3)
Public_Density_cluster_2 <- Public_Density_scaled %>% add_column(cluster = factor(pcc3$cluster))

medoids4 <- as_tibble(Public_Density_scaled[pcc3$medoids, ], rownames = "cluster")

ggplot(Public_Density_cluster_2, aes(x = Population_Density, y = Public_Commute_Percent, color = cluster)) + geom_point()+
  geom_point(data = medoids4, aes(x = Population_Density, y = Public_Commute_Percent, color = cluster), shape = 3, size = 10) + ggtitle("Population Density and percentage of public transportation (PAM)") +theme(plot.title = element_text(hjust = 0.5))



#---------------------- Part 2.3 -----------------------------------------------

Area_Long <- data.frame(PopDensity, CommInc$LongCommutelist)
colnames(Area_Long)[1] <- "Pop_Density"
colnames(Area_Long)[2] <- "Long_Commute_Percent"

ggplot(Area_Long, aes(Pop_Density, Long_Commute_Percent)) +
  geom_point() + ggtitle("Population Density and percentage of long commute time (>45mins) (no cleaning)") +theme(plot.title = element_text(hjust = 0.5))


Area_Long_scaled <- Area_Long[Area_Long$Pop_Density < 30,] %>% scale_numeric()

ggplot(Area_Long_scaled, aes(Pop_Density, Long_Commute_Percent)) +
  geom_point() + ggtitle("Population Density and percentage of long commute time (>45mins) (after cleaning)") +theme(plot.title = element_text(hjust = 0.5))


dist2 <- dist(Area_Long_scaled)
hc2 <- hclust(dist2, method = "average")
plot(hc2)

Area_Long_cluster <- cutree(hc2, k = 3)         
Area_Long_cluster_complete <- Area_Long_scaled %>%
  add_column(cluster = factor(Area_Long_cluster))
                                                            # Similarity between population density and the percentage that people will take longer commute (>45 mins)
ggplot(Area_Long_cluster_complete, aes(Pop_Density, Long_Commute_Percent, color = cluster)) +
  geom_point() + ggtitle("Population Density and percentage of long commute time (>45mins) (HC)") +theme(plot.title = element_text(hjust = 0.5))



pcc4 <- pam(dist2, k = 2)
Area_Long_cluster_2 <- Area_Long_scaled %>% add_column(cluster = factor(pcc4$cluster))

medoids5 <- as_tibble(Area_Long_scaled[pcc4$medoids, ], rownames = "cluster")

ggplot(Area_Long_cluster_2, aes(x = Pop_Density, y = Long_Commute_Percent, color = cluster)) + geom_point()+
  geom_point(data = medoids5, aes(x = Pop_Density, y = Long_Commute_Percent, color = cluster), shape = 3, size = 10) + ggtitle("Population Density and percentage of long commute time (>45mins) (PAM)") +theme(plot.title = element_text(hjust = 0.5))





#====================== Part 3 =================================================

AgeData <- data.frame(c(CensusData[,1:11],CensusData[,40:55]))
oldpercentlist = numeric()

for(i in 1:254){
  nofold = sum(AgeData[i,12:27])
  oldpercentlist[i]  = nofold / AgeData[i,"total_pop"]
}

AgeData$OldPercent <- oldpercentlist
summary(AgeData)

Age_Density <- data.frame(AgeData$median_age,AgeData$median_income)
colnames(Age_Density)[1] <- "Med_Age"
colnames(Age_Density)[2] <- "Med_Income"

ggplot(Age_Density, aes(Med_Age, Med_Income)) +
  geom_point() + ggtitle("Median age and Median income") +theme(plot.title = element_text(hjust = 0.5))


Age_Density_scaled <- Age_Density %>% scale_numeric()

                          
ad <- dist(Age_Density_scaled)      # Similarity between county's population density and its percentage of old people (>60)
ap <- pam(ad, k = 3)              # Use Partitioning Around Medoids
Age_Density_clustered <- Age_Density_scaled %>% add_column(cluster = factor(ap$cluster))
medoids <- as_tibble(Age_Density_scaled[ap$medoids, ], rownames = "cluster")


ggplot(Age_Density_clustered, aes(x = Med_Age, y = Med_Income, color = cluster)) + geom_point() +
  geom_point(data = medoids, aes(x = Med_Age, y = Med_Income, color = cluster), shape = 3, size = 10) + ggtitle("Median age and Median income (PAM)") +theme(plot.title = element_text(hjust = 0.5))


ahc <- hclust(ad, method = "complete")
plot(ahc)

Age_Density_cluster_1 <- cutree(ahc, k = 3)         
Age_Density_cluster_complete <- Age_Density_scaled %>%
  add_column(cluster = factor(Age_Density_cluster_1))
ggplot(Age_Density_cluster_complete, aes(Med_Age, Med_Income, color = cluster)) +
  geom_point() + ggtitle("Median age and Median income (HC)") +theme(plot.title = element_text(hjust = 0.5))


kNNdistplot(Age_Density_scaled , k = 3)
abline(h = 0.8, col = "red")


m <- Mclust(Age_Density_scaled)
plot(m, what = "classification")




pimage(ad, order=order(ap$cluster))   # Cluster Validation




#====================== Part 4 =================================================

#---------------------- Part 4.1 -----------------------------------------------    Case + kmean
PopData <- data.frame(PopDensity,CaseRate)      #Population density relates to case rate(case/total population)

ggplot(PopData, aes(y = CaseRate, x = PopDensity)) + geom_point()  + ggtitle("The county's population density and its averaged confirmed cases (No cleaning)") +theme(plot.title = element_text(hjust = 0.5))

CPopData <- PopData[PopData$PopDensity <= 50, ]     # Remove extreme cases, counties with extreme high 

ggplot(CPopData, aes(y = CaseRate, x = PopDensity)) + geom_point() + ggtitle("The county's population density and its averaged confirmed cases (After cleaning)") +theme(plot.title = element_text(hjust = 0.5))

CPopData_scaled <- CPopData %>% scale_numeric()   # Scale the data frame

km1 <- kmeans(CPopData_scaled, centers = 2, nstart = 10)
CPopData_clustered <- CPopData_scaled %>% add_column(cluster = factor(km1$cluster))
centroids1 <- as_tibble(km1$centers, rownames = "cluster")
ggplot(CPopData_clustered, aes(x = PopDensity, y = CaseRate, color = cluster)) + geom_point()+
  geom_point(data = centroids1, aes(x = PopDensity, y = CaseRate, color = cluster), shape = 4, size = 10)  + ggtitle("The county's population density and its averaged confirmed cases (K-means)") +theme(plot.title = element_text(hjust = 0.5))# Graph with clustering



#---------------------- Part 4.2 -----------------------------------------------    Case + PAM
dd <- dist(CPopData_scaled)   # Use the distance matrix to do the Internal Cluster Validation
pimage(dd, order=order(km1$cluster))

pcc <- pam(dd, k = 2)
CPopData_clustered_2 <- CPopData_scaled %>% add_column(cluster = factor(pcc$cluster))

medoids1 <- as_tibble(CPopData_scaled[pcc$medoids, ], rownames = "cluster")

ggplot(CPopData_clustered, aes(x = PopDensity, y = CaseRate, color = cluster)) + geom_point()+
  geom_point(data = medoids1, aes(x = PopDensity, y = CaseRate, color = cluster), shape = 3, size = 10)  + ggtitle("The county's population density and its averaged confirmed cases (PAM)") +theme(plot.title = element_text(hjust = 0.5))# Graph with clustering


#---------------------- Part 4.3 -----------------------------------------------    Death + kmean
PopData1 <- data.frame(PopDensity,DeathRate)  # Similar to part 1.1, but use death cases for evaluation

ggplot(PopData1, aes(y = DeathRate, x = PopDensity)) + geom_point() + ggtitle("The county's population density and its averaged death (No cleaning)") +theme(plot.title = element_text(hjust = 0.5))


DPopData <- PopData1[PopData1$PopDensity <= 50, ]

ggplot(DPopData, aes(y = DeathRate, x = PopDensity)) + geom_point() + ggtitle("The county's population density and its averaged death (After cleaning)") +theme(plot.title = element_text(hjust = 0.5))

DPopData_scaled <- DPopData %>% scale_numeric()

km2 <- kmeans(DPopData_scaled, centers = 3, nstart = 10)
DPopData_clustered <- DPopData_scaled %>% add_column(cluster = factor(km2$cluster))
centroids2 <- as_tibble(km2$centers, rownames = "cluster")
ggplot(DPopData_clustered, aes(x = PopDensity, y = DeathRate, color = cluster)) + geom_point()+
  geom_point(data = centroids2, aes(x = PopDensity, y = DeathRate, color = cluster), shape = 3, size = 10) + ggtitle("The county's population density and its averaged death cases (K-means)") +theme(plot.title = element_text(hjust = 0.5))# Graph with clustering


#---------------------- Part 4.4 -----------------------------------------------    Death + PAM
dd1 <- dist(DPopData_scaled)
pcc1 <- pam(dd1, k = 3)
DPopData_clustered_2 <- DPopData_scaled %>% add_column(cluster = factor(pcc1$cluster))

medoids2 <- as_tibble(DPopData_scaled[pcc1$medoids, ], rownames = "cluster")

ggplot(DPopData_clustered_2, aes(x = PopDensity, y = DeathRate, color = cluster)) + geom_point() +
  geom_point(data = medoids2, aes(x = PopDensity, y = DeathRate, color = cluster), shape = 3, size = 10) + ggtitle("The county's population density and its averaged death cases (PAM)") +theme(plot.title = element_text(hjust = 0.5))# Graph with clustering


as.matrix(dd1)[1:10, 1:10]
pimage(dd1, order=order(km2$cluster))


