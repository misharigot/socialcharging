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

# Returns the date of the 
toNextWeekStartDate <- function(hours, day) {
  nextMonday <- nextWeekday(1)
  date <- nextMonday + day - 1
  hms <- hms(paste0(hours, ":00:00"))
  ymd_hms(paste(date, hms, sep = "-"))
}

# ToDo: Finish this function, converting prep_start_time, day and pred_hours_elapsed to an actual date for next week
toNextWeekEndDate <- function(hours, day, elapsed) {
  # Return the end date as next week's date
}

