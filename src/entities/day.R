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
    # Add a charging sessions to this day by providing a ChargingSession object 
    # or a list with ChargingSession objects
    addChargingSessions = function(chargingSessions) {
      validTypes <- FALSE
      if (is.list(chargingSessions)) {
        validTypes <- all(as.logical(lapply(chargingSessions, is, "ChargingSession")))
      } else if (is(chargingSessions, "ChargingSession")) {
        validTypes <- TRUE
      } 
      if (validTypes) {
        private$..chargingSessions <- c(private$..chargingSessions, chargingSessions)
      } else {
        warning("Error: object given was not of class 'ChargingSession' or 
                list given contained non-ChargingSession objects.")
      }
      invisible(self)
    },
    hasChargingSessions = function() {
      length(private$..chargingSessions) > 0
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
