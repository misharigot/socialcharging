# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
config <- config::get(file = "../config.yml")
source(config$baseClean)
source(config$multiplotHelper)
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
    filter(smart_charging == 'Yes')
}

a <- getIsSmart()

# Returns table with not-smart rows
getIsNotSmart <- function() {
  getRemovedNa() %>% 
    filter(smart_charging == 'No')
}

# Plot functions ----------------------------------------------------------

# checking the amount of usages on the smart- and non smart chargers
plotSmart <- function() {
  p <- ggplot(getRemovedNa(), aes(x = smart_charging)) + 
    geom_bar() + 
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) + 
    labs(x = "smart charging")
  return(p)
}

# IsSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotKwhElapsedSmart <- function() {
  p <-  ggplot(getIsSmart(), aes(x = hours_elapsed, y = charged_kwh)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "session time in hours", y = "kWh charged") +
    ggtitle("Smart charging station")
  return(p)
}

# IsSmart scatterplot displaying a possible relation between the effective charging time and session time
plotEffectiveChargingHourElapsedSmart <- function() {
  p <- ggplot(getIsSmart(), aes(x = hours_elapsed, y = effective_charging_hours)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "elapsed time in hours", y = "effective charging time in hours") +
    #xlim(0,40) +
    ggtitle("Smart charging station")
  return(p)
}

# NotSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotKwhElapsed <- function() {
  p <- ggplot(getIsNotSmart(), aes(x = hours_elapsed, y = charged_kwh)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "session time in hours", y = "kWh charged") +
    ggtitle("Normal charging station")
  return(p)
}

# NotSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotEffectiveChargingHourElapsed <- function() {
  p <- ggplot(getIsNotSmart(), aes(x = hours_elapsed, y = effective_charging_hours)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "elapsed time in hours", y = "effective charging time in hours") +
    #xlim(0,40) +
    ggtitle("Normal charging station")
  return(p)
}

# Calls -------------------------------------------------------------------
plotSmart()
plotKwhElapsedSmart()
plotEffectiveChargingHourElapsedSmart()
plotKwhElapsed()
plotEffectiveChargingHourElapsed()

multiplotHelper(plotKwhElapsedSmart(),plotKwhElapsed())
multiplotHelper(plotEffectiveChargingHourElapsedSmart(),plotEffectiveChargingHourElapsed())
