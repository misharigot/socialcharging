# Linear regression model for predicting session end time based on user classification
library(ggplot2)
library(config)
library(readr)
library(lubridate)
library(tidyr)
library(purrr)
library(corrplot)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)
source("./src/models/station_classification.R")

df <- read_csv2(config$scDataset, col_names = FALSE)

df <- cleanSecondDf(df)

# Data preperation --------------------------------------------------------

prepareDataForStationPred <- function(df, userClass){
  
  # Create dataframe with station classifications 
  stationClassifications <- cleanStationDf(df)
  
  # Add station classifications to df
  df$station_class <- stationClassifications$stationClass[match(df$longitude + df$latitude, stationClassifications$longitude + stationClassifications$latitude)]
 
  # Filter corrupted data (most of the filtering is already done in regression_user_class)
  df <- df %>% filter(!is.na(station_class))
  
  return(df)
}

# Create linear model ----------------------------------------------------

createLinearModelDataStation <- function(df){
  minimumSessions <- 10
  # if the prediction has to be made for all classifications individually
  i <- 1
  stationClassificationsUnique <- unique(as.character(df$station_class))
  print(stationClassificationsUnique)
  
  idsWithPreds <- data.frame("session_id" = numeric(0), "station_pred" = numeric(0))
  
  # Predict session time for each profile
  while (i <= length(stationClassificationsUnique)) {
    # Create a df for current user classification 
    sessionsWithSpecificClass <- df %>%
      filter(station_class == stationClassificationsUnique[i])
    
    if (nrow(sessionsWithSpecificClass) > minimumSessions) {
      # Create linear model
      lm_df <- lm(hours_elapsed ~ hour + charged_kwh + kw_charge_point_speed, data = sessionsWithSpecificClass)
      
      # Add predictions to sessions
      sessionsWithSpecificClass$station_pred <- predict(lm_df, sessionsWithSpecificClass)
      
      dfToMerge <- sessionsWithSpecificClass[, c("session_id", "station_pred")]
      
      # Append dfToMerge to idsWithPreds
      idsWithPreds <- rbind(idsWithPreds, dfToMerge)
    }
    i <- i + 1
  }
  return(idsWithPreds)
}
