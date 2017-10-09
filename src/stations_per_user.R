library(readr)
library(ggplot2)
library(config)
library(dplyr)
config <- config::get()
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

#Select the amount of unique sessions per user per charging station 
uniqueUsersPerStationSubset <- df %>%
  select(user_id, latitude, longitude) %>%
  arrange(user_id) %>% 
  distinct(user_id,longitude,latitude) 

#Distributed amount of charging stations used per user in Januari
#TODO: How is this distributed in percentages?
countOfUsersPerDifferentStations <- uniqueUsersPerStationSubset %>%
  group_by(user_id) %>%
  summarise(charging_station_count = n()) %>%
  group_by(charging_station_count) %>%
  summarise(user_amount = n())

#Check per week?
#IF user used more than 1 chargingstation -> check distances between charging stations
#Distributed between users (also calculate mean)


#Plot countOfUsersPerDifferentStations as bar graph
plotUsersPerDifferentStations <- ggplot(data=countOfUsersPerDifferentStations,aes(x=charging_station_count , y=user_amount)) + geom_bar(stat="identity")
plotUsersPerDifferentStations + labs(x = "Amount of charging stations used", y = "Amount of users")
ggtitle("Distributed amount of charging stations used per user in Januari")






