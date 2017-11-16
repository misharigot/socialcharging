#68-station classification
#classify the station by how important they are 
#using the information about shiny map 

library(ggplot2)
library(config)
library(readr)

config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanSecondDf(df)


# trim data ----------------------------------------------------------

cleanDataForStation <- function(x) {
  
  df <- df %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed))
  totalHours <- interval(min(df$start_date), max(df$end_date)) / 3600
  df <- df %>%
    group_by(longitude, latitude) %>%
    summarise(address = first(address),
              outlets = first(outlets),
              total_sessions = n(),
              total_users = n_distinct(user_id),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed)
    )
}

# classification ----------------------------------------------------------

# using five number summary give the value for station
givePoint <- function(x) {
  kwhSummary <- summary(x)
  med <- kwhSummary[3]
  ifelse (x > med, "H", "L")
}

#make a station table
stationPointTable <- function(x) {
  df <- cleanDataForStation(df)
  df$charging <- givePoint(df$total_charged)
  df$occupation <- givePoint(df$total_hours_elapsed)
  df$user_amount <- givePoint(df$total_users)
  df <- df %>%  select(latitude, address, occupation, user_amount, charging)
}

#change class name fancy
changeName <- function(x) {
  ifelse (x == "HHH", "LadyOfTheEvening",
          ifelse ( x == "HHL", "ParkingSpace",
                   ifelse (x == "HLL", "MarriedToThisStation",
                           ifelse (x == "HLH", "LateNightCharging",
                                   ifelse(x == "LHH", "WorkerBee",
                                          ifelse(x == "LLH", "PowerBank",
                                                 ifelse(x == "LLL", "ForeverAlone", "HitandRun")
                                          )
                                   )
                           )
                   )
          )
  )
}

# make the final dataframe
makestationDf <- function(df) {
  stationDf <- stationPointTable(df)
  stationDf$stationClass <- paste(stationDf$occupation, stationDf$user_amount, stationDf$charging, sep = "")
  stationDf$stationClass <- changeName(stationDf$stationClass)
  return(stationDf)
}


# distribution ------------------------------------------------------------

# show the distribution of the station class
stationClassDis <- function(df){
  df %>%
    group_by(stationClass) %>%
    summarise(num = n()) %>%
    mutate(stationClass = factor(stationClass, levels = stationClass[order(num)]))  
}
showDistribution <- function(df){
  ggplot(stationClassDis(df), aes(x = stationClass, y = num)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66bb6a") +
    coord_flip() +
    ggtitle("Show the station Class number")
}
showDistribution(makestationDf(df))
