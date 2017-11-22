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
source("./src/models/user_class.R")

df <- read_csv2(config$scDataset, col_names = FALSE)

df <- cleanSecondDf(df)

# Data preperation --------------------------------------------------------

prepareDataForLM <- function(df, userClass){
  
  # Create dataframe with user classifications 
  userClassifications <- userClassificationDf(sessionClassificationDf(cleanDf(df)))
  
  # Add user classifications to df
  df$user_class <- userClassifications$class[match(df$user_id, userClassifications$user_id)]
  
  # Makes subset of the sessions for the specified user classification
  if (userClass != 0) {
    specificUserSessions <- subset(userClassifications, userClassifications$class == userClass)
    # filter on users with chosen classification
    df <- df %>%
      filter(user_id %in% specificUserSessions$user_id )
  }
  
  # Filter corrupted data
  df <- df %>%
    filter(
      !is.na(hours_elapsed),
      hours_elapsed > 0 & hours_elapsed <= 24,
      !is.na(start_date),
      !is.na(end_date),
      !is.na(car),
      !is.na(charged_kwh),
      !is.na(user_class)
    )
  
  # Gets the day of the week from the date
  df$dayOfWeek <- weekdays(as.Date(df$start_date))
  # Gets the starting hour of session
  df$hour <- as.numeric(format(round(df$start_date, "hours"), format = "%H"))
  return(df)
}

# Create linear model ----------------------------------------------------

createLinearModelData <- function(df, isTest){
  minimumSessions <- 10
  # if the prediction has to be made for all classifications individually
  i <- 1
  userClassificationsUnique <- unique(as.character(cleanDf$user_class))
  print(userClassificationsUnique)
  
  idsWithPreds <- data.frame("session_id" = numeric(0), "user_pred" = numeric(0))
  
  # Predict session time for each profile
  while (i <= length(userClassificationsUnique)) {
    # Create a df for current user classification 
    sessionsWithSpecificClass <- df %>%
      filter(user_class == userClassificationsUnique[i])
    
    if (nrow(sessionsWithSpecificClass) > minimumSessions) {
      # Create linear model
      lm_df <- lm(hours_elapsed ~ charged_kwh, data = sessionsWithSpecificClass)
      
      # Add predictions to sessions
      sessionsWithSpecificClass$user_pred <- predict(lm_df, sessionsWithSpecificClass)
      
      dfToMerge <- sessionsWithSpecificClass[, c("session_id", "user_pred")]
      
      # Append dfToMerge to idsWithPreds
      idsWithPreds <- rbind(idsWithPreds, dfToMerge)
    }
    i <- i + 1
  }
  return(idsWithPreds)
}
