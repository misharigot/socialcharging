library(dplyr)
library(lubridate)

source("src/entities/week_schedule.R")
source("src/entities/day.R")
source("src/entities/charging_session.R")

# PUBLIC API ------------------------------------------------------------------------------------------------------

# Converts a df containing the predictions for a week for a user classification to a WeekSchedule object.
convertWeekPredToSchedule <- function(dfWithWeekPredictions) {
  weekschedule <- weekScheduleFactory$new()
  for (index in 1:nrow(dfWithWeekPredictions)) {
    dayAsString <- numericToDayString(dfWithWeekPredictions[index, "day_of_week"])
    
    session <- dfWithWeekPredictions[index, ]
    chargingSession <- convertRowsToChargingSessions(session)
    weekschedule$addChargingSessions(dayAsString, chargingSession)
  }
  return(weekschedule)
}

# PRIVATE FUNCTIONS -----------------------------------------------------------------------------------------------

# Create dummy session predictions
createDummyWeekForClass <- function() {
  amountOfDummySessions = 15
  canContainDuplicates = FALSE
  if (amountOfDummySessions > 7) {
    canContainDuplicates = TRUE
  }
  data.frame(user_class = sample(1, amountOfDummySessions, replace = TRUE),
             day_of_week = sample(1:7, amountOfDummySessions, replace = canContainDuplicates),
             start_time = (sample.int(2400, amountOfDummySessions, replace = TRUE)) / 100, 
             hours_elapsed = (sample.int(2400, amountOfDummySessions, replace = TRUE)) / 100,
             charged_kwh = (sample.int(3000, amountOfDummySessions, replace = TRUE)) / 100
             )
}

# Convert a number between 1 and 7 to the corresponding day as a string.
numericToDayString <- function(numeric) {
  if (numeric < 1 | numeric > 7) {
    warning("Invalid number, give number between 1 and 7.")
    return(NULL)
  }
  switch(as.character(numeric),
         "1" = "monday",
         "2" = "tuesday",
         "3" = "wednesday",
         "4" = "thursday",
         "5" = "friday",
         "6" = "saturday",
         "7" = "sunday"
         )
}

# Convert session rows to ChargingSession objects.
convertRowsToChargingSessions <- function(rows) {
  filteredRows <- rows %>% select(start_time, hours_elapsed, charged_kwh)
  chargingSessions <- apply(filteredRows, 1, function(x) {
    chargingSessionFactory$new(x[["start_time"]], x[["hours_elapsed"]], x[["charged_kwh"]])
  })
}

# Test the functionality
test <- function() {
  dummyDf <- createDummyWeekForClass()
  schedulesForClassOne <- convertWeekPredToSchedule(dummyDf)
  print(schedulesForClassOne)
}

test()
