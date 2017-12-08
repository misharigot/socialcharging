library(R6)
source("src/entities/day.R")
source("src/entities/charging_session.R")

weekScheduleFactory <- R6Class(
  "WeekSchedule",
  private = list(
    ..monday = dayFactory$new("Monday"),
    ..tuesday = dayFactory$new("Tuesday"),
    ..wednesday = dayFactory$new("Wednesday"),
    ..thursday = dayFactory$new("Thursday"),
    ..friday = dayFactory$new("Friday"),
    ..saturday = dayFactory$new("Saturday"),
    ..sunday = dayFactory$new("Sunday")
  ),
  public = list(
    print = function() {
      cat("\n~~~~~ Week schedule: ~~~~~\n")
      print(private$..monday)
      cat("\n")
      print(private$..tuesday)
      cat("\n")
      print(private$..wednesday)
      cat("\n")
      print(private$..thursday)
      cat("\n")
      print(private$..friday)
      cat("\n")
      print(private$..saturday)
      cat("\n")
      print(private$..sunday)
      cat("\n")
      invisible(self)
    },
    addChargingSession = function(day, chargingSession) {
      index <- paste("..", day, sep = "")
      private[[index]]$addChargingSession(chargingSession)
      invisible(self)
    }
  )
)
