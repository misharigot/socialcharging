library(lubridate)

# Return a date string of the next weekday specified:
# Example: wday = 1 (Monday)
# Returns the date of the next sunday based on current time
nextWeekday <- function(wday) {
  if (wday > 0 & wday < 8) {
    today <- date(now())
    nextWeekDay <- today + 7
    ceiling_date(nextWeekDay, unit = "day") + wday - 1
  } else {
    warning("Please give a number between 1 (monday) and 7 (sunday)")
  }
}

hoursToNextWeekDate <- function(hours, day) {
  
}