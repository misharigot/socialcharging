# Writes a csv to data folder with predictions
library(config)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)
source("./src/models/regression_user_class.R")
source("./src/models/regression_station_class.R")

generatePredictionCsv <- function() {
  df <- read_csv2(config$scDataset, col_names = FALSE)
  df <- cleanSecondDf(df)
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
