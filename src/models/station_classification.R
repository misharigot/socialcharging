#68-station classification
#classify the station by how important they are 
#using the information about shiny map 

library(ggplot2)
library(config)
library(readr)
library(dplyr)
config <- config::get(file = "config.yml")
source(config$baseClean)

# trim data and classification----------------------------------------------------------

# using five number summary give the value for station
divideCategory <- function(x) {
  kwhSummary <- summary(x)
  med <- kwhSummary[3]
  outVector <- rep(0, length(x))
  for (i in 1:length(x)) {
    if(x[i] > med) {
      outVector[i] <- "H"  
    } else {
      outVector[i] <- "L"
    }
  }
  return(outVector)
}

#change class name fancy
changeName <- function(x) {
  res <- switch(x,
                "HHH" = "LadyOfTheEvening",
                "HHL" = "ParkingSpace",
                "HLL" = "MarriedToThisStation",
                "HLH" = "LateNightCharging",
                "LHH" = "WorkerBee",
                "LLH" = "PowerBank",
                "LLL" = "ForeverAlone",
                "LHL" = "HitandRun")
  return(res)
}

#dat cleaning and classification
cleanStationDf <- function(df) {
  df <- df %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed)) %>% 
    group_by(longitude, latitude) %>%
    summarise(address = first(address),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed),
              total_users = n_distinct(user_id))
  df$charging <- divideCategory(df$total_charged)
  df$occupation <- divideCategory(df$total_hours_elapsed)
  df$user_amount <- divideCategory(df$total_users)
  df$stationClass <- paste(df$occupation, df$user_amount, df$charging, sep = "")
  df$stationClass <- lapply(df$stationClass, changeName)
  
  return(df) 
}

# distribution ------------------------------------------------------------

# show the distribution of the station class
countClass <- function(df){
  df$stationClass <- as.character(df$stationClass)
  df %>%
    group_by(stationClass) %>%
    summarise(num = n()) %>%
    mutate(stationClass = factor(stationClass, levels = stationClass[order(num)]))  
}

# make a plot for showing distribution
showDistribution <- function(scData){
  ggplot(scData, aes(x = stationClass, y = num)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66bb6a") +
    coord_flip() +
    ggtitle("Show the station Class number")
}

# call --------------------------------------------------------------------

showDistributionPlot <- function(df) {
  showDistribution(countClass(cleanStationDf(df))) 
}
