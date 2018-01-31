# This class has several helper functions regarding date, containing custom functions that lubridate doesn't provide.
library(lubridate)

# Get weeknumber from date starting on monday
# Created a custom function, because lubridate's week function starts the week on sunday
getWeekNumber <- function(date) {
  strftime(as.Date(date), format = "%V")
}

# Gets weekday from the date starting on monday
# Created a custom function, because lubridate's day function starts the counting on sunday
getDay <- function(date)  {
  strftime(as.Date(date), format = "%u")
}

# Return a date string of the next weekday specified:
# Example: wday = 1 (Monday)
# Returns the date of the next sunday based on current time
nextWeekday <- function(wday) {
  if (wday > 0 & wday < 8) {
    today <- date(now())
    nextWeekDay <- today + 7
    ceiling_date(nextWeekDay, unit = "day") + wday - wday(today)
  } else {
    warning("Please give a number between 1 (monday) and 7 (sunday)")
  }
}

# Returns the start date time of a session, based on the starting hour and the day (1-7).
toNextWeekStartDate <- function(startingHour, day) {
  date <- nextWeekday(day)
  if (startingHour != 0) {
    hms <- hms(paste0(startingHour, ":00:00"))
  } else {
    hms <- "00:00:00"
  }
  dateString <- paste(date, hms, sep = "-")
  ymd_hms(dateString)
}

# Returns the end date time of a session, based on the starting hour, the day (1-7) and the time elapsed (hour decimal).
toNextWeekEndDate <- function(startingHour, day, elapsed) {
  startDate <- toNextWeekStartDate(startingHour, day)
  endDate <- startDate + hours(floor(elapsed)) + minutes(getMinutes(elapsed))
}

# Returns the minutes of a decimal hour (hours = 1.1 returns 6 minutes)
getMinutes <- function(hours) {
  if (isDecimal(hours)) {
    minutesFraction <- hours - floor(hours)
    result <- minutesFraction * 60
    return(floor(result))
  } else {
    return(0)
  }
}

# Check if a number is a decimal number
isDecimal <- function(number) {
  number %% 1 != 0
}

# Strips date and returns time
stripDate <- function(datetime, dateTimeFormat){
  x <- strptime(datetime, format = dateTimeFormat)
  return(format(x, "%H:%M:%S"))
}

toHourAndMinutes <- function(decimal){
  hourAndMinutes <- paste(floor(decimal), round((decimal - floor(decimal)) * 60), sep=":")
  
  return(paste0(hourAndMinutes, ":00"))
}

# Returns the number of the day of the month
getWeekOfMonth <- function(start_date){
  weekOfMonth <- ceiling(day(start_date) / 7)
  
  return (weekOfMonth)
}
