library(R6)
chargingSessionFactory <- R6Class(
  "ChargingSession",
  private = list(
    ..startTime = NA,
    ..durationHours = NA,
    ..chargedKwh = NA
  ),
  public = list(
    initialize = function(startTime = NA, durationHours = NA, chargedKwh = NA) {
      private$..startTime = startTime
      private$..durationHours = durationHours
      private$..chargedKwh = chargedKwh
    },
    print = function() {
      cat("\tstartTime: ", private$..startTime, "\n", sep = "")
      cat("\tdurationHours: ", private$..durationHours, "\n", sep = "")
      cat("\tchargedKwh: ", private$..chargedKwh, "\n", sep = "")
      invisible(self)
    }
  ),
  active = list()
)

# Check if object is an instance of class ChargingSession and R6
is.ChargingSession = function(object) {
  class(object)[[1]] == "ChargingSession" & class(object)[[2]] == "R6"
}
