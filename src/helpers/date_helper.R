library(lubridate)

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
  nextMonday <- nextWeekday(1)
  date <- nextMonday + day - 1
  hms <- hms(paste0(startingHour, ":00:00"))
  ymd_hms(paste(date, hms, sep = "-"))
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

formatKwh <- function(kwh){
  paste0(kwh, " kwh")
}
