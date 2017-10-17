#How many users use more than one charging station? If so how far are those charging stations apart?
library(readr)
library(ggplot2)
library(config)
library(dplyr)
library(geosphere)
        
config <- config::get()
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)



# Table functions ---------------------------------------------------------


#Select the amount of unique sessions per user per charging station 
uniqueUsersPerStation <- function(){ 
  df %>%
  select(user_id, latitude, longitude) %>%
  arrange(user_id) %>% 
  distinct(user_id,longitude,latitude) 
}

#Distributed amount of charging stations used per user in Januari
#TODO: How is this distributed in percentages? Check per week?
countOfUsersPerAmountOfStationsUsed <- function(){
  uniqueUsersPerStation() %>%
  group_by(user_id) %>%
  summarise(charging_stations_used = n()) %>%
  group_by(charging_stations_used) %>%
  summarise(user_amount = n())
}

# Plot functions ----------------------------------------------------------


#Plot countOfStationsUsed as bar graph
plotUsersPerDifferentStations <- function(){
    ggplot(data=countOfUsersPerAmountOfStationsUsed(),aes(x=charging_stations_used, y=user_amount)) + 
    geom_bar(stat="identity") +
    labs(x = "Amount of charging stations used", y = "Amount of users") +
    ggtitle("Distributed amount of charging stations used per user in Januari")
}

# Calls -------------------------------------------------------------------
plotUsersPerDifferentStations()
