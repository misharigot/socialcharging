library(R6)
source("src/entities/day.R")
source("src/entities/charging_session.R")

weekScheduleFactory <- R6Class(
  "WeekSchedule",
  private = list(
    ..days = list(
      monday = dayFactory$new("Monday"),
      tuesday = dayFactory$new("Tuesday"),
      wednesday = dayFactory$new("Wednesday"),
      thursday = dayFactory$new("Thursday"),
      friday = dayFactory$new("Friday"),
      saturday = dayFactory$new("Saturday"),
      sunday = dayFactory$new("Sunday")
    )
  ),
  public = list(
    print = function() {
      cat("\n~~~~~ Week schedule: ~~~~~\n")
      print(private$..days$monday)
      cat("\n")
      print(private$..days$tuesday)
      cat("\n")
      print(private$..days$wednesday)
      cat("\n")
      print(private$..days$thursday)
      cat("\n")
      print(private$..days$friday)
      cat("\n")
      print(private$..days$saturday)
      cat("\n")
      print(private$..days$sunday)
      cat("\n")
      invisible(self)
    },
    addChargingSessions = function(day, chargingSessions) {
      private$..days[[day]]$addChargingSessions(chargingSessions)
      invisible(self)
    },
    hasChargingSessions = function() {
      bools = c()
      for (day in private$..days) {
        bools <- c(bools, day$hasChargingSessions())
      }
      TRUE %in% bools
    },
    toDataVisDf = function() {
      # 
      # data.frame(
      #   content = paste0(selectData$charged_kwh, " kwh"),
      #   start = paste0("2017-12-", private$),
      #   end = selectData$end_date
      # )
    }
  ),
  active = list(
    days = function() private$..days
  )
)
