library(R6)
source("src/entities/charging_session.R")

dayFactory <- R6Class(
  "Day",
  private = list(
    ..name = NA,
    ..chargingSessions = list()
  ),
  public = list(
    initialize = function(name = NA) {
      self$name <- name
    },
    print = function() {
      cat("Day: ", private$..name, "\n", sep = "")
      cat(length(private$..chargingSessions), " charging sessions\n", sep = "")
      lapply(private$..chargingSessions, function(x) {
        cat("\n")
        print(x)
      })
      invisible(self)
    },
    # Add a charging session to this day by providing a chargingSession object
    addChargingSession = function(chargingSession) {
      if (is.ChargingSession(chargingSession)) {
        private$..chargingSessions <- c(private$..chargingSessions, chargingSession)
      } else {
        message("Error: object given was not of class 'ChargingSession'.")
      }
      invisible(self)
    }
  ),
  active = list(
    name = function(value) {
      if (missing(value)) {
        private$..name
      } else {
        private$..name = value
      }
    },
    chargingSessions = function(chargingSessionsList) {
      if (missing(chargingSessionsList)) {
        private$..chargingSessions
      } else {
        private$..chargingSessions = chargingSessionsList
      }
    }
  )
)
