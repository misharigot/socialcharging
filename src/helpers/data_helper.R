library(config)
library(dplyr)
library(xgboost)
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
  cleanedDf <- prepareDataForUserPred(df)
  cleanedDf$smartCharging <- ifelse(cleanedDf$smart_charging == "Yes", 1, 0)
  
  # testingGamPred <- createGAMModelDataUser(df = cleanedDf, formula = hours_elapsed ~ hour + charged_kwh, minimumSessions = 5)
  sessionsIdsWithPreds <- createLinearModelDataUser(cleanedDf)
  result <-
    base::merge(cleanedDf, sessionsIdsWithPreds, by = "session_id")
  
  # Creates a dataframe with predictions based on station classification
  cleanedDf <- prepareDataForStationPred(result)
  sessionsIdsWithPreds <- createLinearModelDataStation(cleanedDf)
  result <-
    base::merge(cleanedDf, sessionsIdsWithPreds, by = "session_id")
  
  # Change numeric user classifications to descriptive names
  result$user_class <- as.character(result$user_class)
  result$user_class <-
    lapply(result$user_class, changeToDescriptiveName)
  
  # Writes dataframe to csv file
  result <- as.data.frame(lapply(result, unlist))
  write.csv(result, config$dataFolder)
}


# Helper functions ----------------------------------------------------
changeToDescriptiveName <- function(x) {
  res <- switch(
    x,
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
    "-1" = "Longer24Hours"
  )
  return(res)
}

# Returns a data frame with dummy predicted sessions to fill the timeline with.
getDummyPredictedSessions <- function() {
  df <- data.frame(
    "start_time_class" = rNum(10, 300),
    "hours_elapsed_class" = rNum(10, 300),
    "kwh_class" = rNum(10, 300),
    "day" = rNum(7, 300),
    "pred_start_time" = rNum(24, 300),
    "pred_hours_elapsed" = rNum(24, 300),
    "pred_kwh" = rNum(10, 300)
  )
}

# Returns a vector of random numbers between 1 and x.
rNum <- function(x, size) {
  sample(1:x, size, replace = T)
}

# Convert the predicted sessions DF from extended_classification.R to data usable by the timeline visualisation library.
convertSessionsToTimelineData <- function(predictedSessions) {
  predictedSessions %>%
    mutate(day = as.numeric(day)) %>%
    filter(pred_hours_elapsed > 0) %>%
    rowwise() %>%
    mutate(
      start_datetime = toNextWeekStartDate(pred_start_time, day),
      end_datetime = toNextWeekEndDate(pred_start_time, day, pred_hours_elapsed),
      formatted_kwh = formatKwh(pred_kwh),
      efficiency = 100 / (pred_hours_elapsed * 11) * pred_kwh,
      # 11 is the charging speed
      rounded_efficiency = paste0("eff-", plyr::round_any(efficiency, 20, f = ceiling))
    )
}

# Format a kWh number to a presentable string.
formatKwh <- function(kwh) {
  kwh <- round(kwh, digits = 2)
  paste0(kwh, " kWh")
}

test <- function(cleanedDf) {
  
  cleanedDf$IsWorkDay <-
    ifelse(cleanedDf$dayOfWeekNo >= 1 & cleanedDf$dayOfWeekNo < 6, 1, 0)
  
  cleanedDf$IsWorkingHours <-
    ifelse(cleanedDf$hour > 7 & cleanedDf$hour < 18, 1, 0)
  
  cleanedDf.filtered <- cleanedDf %>%
    select("dayOfWeekNo", "hour", "IsWorkDay", "IsWorkingHours")
  
  # split data 25% test, 75% train
  size = floor(0.25 * nrow(cleanedDf))
  sizeNext = size + 1
  total = nrow(cleanedDf)
  
  trainDf <- cleanedDf.filtered[sizeNext:total, ]
  trainDf.label <- cleanedDf[sizeNext:total, ]$hours_elapsed
  
  testDf <- cleanedDf.filtered[1:size, ]
  
  testDf$hours_elapsed <- NULL
  
  cv <- xgb.cv(
    data = as.matrix(trainDf),
    label = trainDf.label,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear",
    eta = 0.3,
    max_depth = 6,
    early_stopping_rounds = 10
  )
  
  # Get the evaluation log
  elog <- cv$evaluation_log
  
  # Determine and print how many trees minimize training and test error
  elog <- elog %>%
    summarize(
      ntrees.train = which.min(train_rmse_mean),
      # find the index of min(train_rmse_mean)
      ntrees.test  = which.min(test_rmse_mean)
    )   # find the index of min(test_rmse_mean)
    
  
    test_model_xgb <- xgboost(data = as.matrix(trainDf), # training data as matrix
                              label = trainDf.label,  # column of outcomes
                              nrounds = elog$ntrees.train,       # number of trees to build
                              objective = "reg:linear", # objective
                              eta = 0.3,
                              depth = 6,
                              verbose = 0  # silent
    )
    
    testDf$pred_hours_elapsed <- predict(test_model_xgb, as.matrix(testDf))
    testDf$actual_hours <- trainDf.label <- cleanedDf[1:size, ]$hours_elapsed
}
