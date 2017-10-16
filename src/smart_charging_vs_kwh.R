# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

# Returns table with na values removed on end_date and charged_kwh
getRemovedNa <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh))
}

# Returns table with smart rows
getIsSmart <- function() {
  getRemovedNa() %>%
    filter(smart_charging == "Yes")
}

# Returns table with not-smart rows
getIsNotSmart <- function() {
  getRemovedNa() %>%
    filter(smart_charging == "No")
}

# Plot functions ----------------------------------------------------------

# checking the amount of usages on the smart- and non smart chargers
plotSmart <- function() {
  p <- ggplot(getRemovedNa(), aes(x = smart_charging)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    labs(x = "smart charging")
  return(p)
}

# IsSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotKwhElapsedSmart <- function() {
  p <-  ggplot(getIsSmart(), aes(y = charged_kwh, x = hours_elapsed)) +
    geom_point(alpha = 0.3) + geom_smooth() +
    labs(x = "session time in hours", y = "kWh charged")
  return(p)
}

# IsSmart scatterplot displaying a possible relation between the effective charging time and session time
plotEffectiveChargingHourElapsedSmart <- function() {
  p <- ggplot(getIsSmart(), aes(y = effective_charging_hours, x = hours_elapsed)) +
    geom_point(alpha = 0.3) +
    labs(x = "effective charging time in hours", y = "elapsed time in hours")
  return(p)
}

# NotSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotKwhElapsed <- function() {
  p <- ggplot(getIsNotSmart(), aes(y = charged_kwh, x = hours_elapsed)) +
    geom_point(alpha = 0.3) + geom_smooth() +
    labs(x = "session time in hours", y = "kWh charged")
  return(p)
}

# NotSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotEffectiveChargingHourElapsed <- function() {
  p <- ggplot(getIsNotSmart(), aes(y = effective_charging_hours, x = hours_elapsed)) +
    geom_point(alpha = 0.3) +
    labs(x = "effective charging time in hours", y = "elapsed time in hours")
  return(p)
}

# Calls -------------------------------------------------------------------

plotSmart()
plotKwhElapsedSmart()
plotEffectiveChargingHourElapsedSmart()
plotKwhElapsed()
plotEffectiveChargingHourElapsed()
