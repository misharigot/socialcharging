library(config)
library(dplyr)
config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

source("./src/models/regression_user_class.R")
source("./src/models/regression_station_class.R")
source("src/helpers/date_helper.R")

# Writes a csv to data folder with predictions
generatePredictionCsv <- function() {
  if (file.exists(config$dataFolder)) {
    file.remove(config$dataFolder) 
  }
  
  df <- read_csv2(config$scDataset, col_names = FALSE)
  df <- cleanDataframe(df)
  
  # Creates a dataframe with predictions based on user classification 
  cleanDf <- prepareDataForUserPred(df)
  sessionsIdsWithPreds <- createLinearModelDataUser(cleanDf)
  result <- base::merge(cleanDf, sessionsIdsWithPreds, by = "session_id")
  
  # Creates a dataframe with predictions based on station classification 
  cleanDf <- prepareDataForStationPred(result)
  sessionsIdsWithPreds <- createLinearModelDataStation(cleanDf)
  result <- base::merge(cleanDf, sessionsIdsWithPreds, by = "session_id")
  
  # Change numeric user classifications to descriptive names
  result$user_class <- as.character(result$user_class)
  result$user_class <- lapply(result$user_class, changeToDescriptiveName)
  
  # Writes dataframe to csv file
  result <- as.data.frame(lapply(result, unlist))
  write.csv(result, config$dataFolder)
}


# Helper functions ----------------------------------------------------
changeToDescriptiveName <- function(x) {
  res <- switch(x,
                "1" = "MorningMorning",
                "2" = "MorningAfternoon",
                "3" = "MorningEvening",
                "4" = "MorningNight",
                "5" = "AfternoonMorning",
                "6" = "AfternoonAfternoon",
                "7" = "AfternoonEvening",
                "8" = "AfternoonNight",
                "9" = "EveningMorning",
                "10" = "EveningAfternoon",
                "11" = "EveningEvening",
                "12" = "EveningNight",
                "13" = "NightMorning",
                "14" = "NightAfternoon",
                "15" = "NightEvening",
                "16" = "NightNight",
                "-1" = "Longer24Hours")
  return(res)
}

# Returns a data frame with dummy predicted sessions to fill the timeline with.
getDummyPredictedSessions <- function() {
  df <- data.frame("start_time_class" = rNum(10, 300), 
                   "hours_elapsed_class" = rNum(10, 300),
                   "kwh_class" = rNum(10, 300),
                   "day" = rNum(7, 300),
                   "pred_start_time" = rNum(24, 300),
                   "pred_hours_elapsed" = rNum(24, 300),
                   "pred_kwh" = rNum(10, 300))
}

# Returns a vector of random numbers between 1 and x.
rNum <- function(x, size) {
  sample(1:x, size, replace = T)
}

convertSessionsToTimelineData <- function(df) {
  df %>% 
    mutate(day = as.numeric(day)) %>%
    filter(pred_hours_elapsed > 0) %>%
    mutate(start_datetime = toNextWeekStartDate(pred_start_time, day),
                end_datetime = toNextWeekEndDate(pred_start_time, day, pred_hours_elapsed),
                formatted_kwh = formatKwh(pred_kwh)
                )
}
