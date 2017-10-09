# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
config <- config::get(file = "../config.yml")
source(config$baseClean)

# TODO: write functions for plots
# TODO: chain plots correctly
# TODO: add titles to plots
# TODO: add calls

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

# Returns table with na values removed on end_date and charged_kwh
GetRemovedNa <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh))
}

# Returns table with smart rows
GetIsSmart <- function() {
  GetRemovedNa() %>%
    filter(smart_charging == 'Yes')
}

# Returns table with not-smart rows
GetIsNotSmart <- function() {
  GetRemovedNa() %>% 
    filter(smart_charging == 'No')
}

# Plot functions ----------------------------------------------------------

# checking the amount of usages on the smart- and non smart chargers
PlotSmart <- function() {
  p <- ggplot(data = GetRemovedNa(), aes(x = smart_charging)) + 
    geom_bar() + 
    geom_text(stat = 'count', aes(label = ..count..), vjust = -1) + 
    labs(x = "smart charging")
  return(p)
}

# plot 1 

plotSmartChargers1 <- ggplot(GetIsSmart(), aes(y = charged_kwh, x = hours_elapsed)) + geom_point(alpha = 0.3) + geom_smooth()
plotSmartChargers1 + labs(x = "session time in hours", y = "kWh charged")

# plot 1.1
plotSmartChargers2 <- ggplot(GetIsSmart(), aes(y = effective_charging_hours, x = hours_elapsed)) + geom_point(alpha = 0.3)
plotSmartChargers2 + labs(x = "effective charging time in hours", y = "elapsed time in hours")

# plot 2
plotNonSmartChargers1 <- ggplot(GetIsNotSmart(), aes(y = charged_kwh, x = hours_elapsed)) + geom_point(alpha = 0.3) + geom_smooth()
plotNonSmartChargers1 + labs(x = "session time in hours", y = "kWh charged")

# plot 2.1
plotNonSmartChargers2 <-  ggplot(GetIsNotSmart(), aes(y = effective_charging_hours, x = hours_elapsed)) + geom_point(alpha = 0.3) 
plotNonSmartChargers2 + labs(x = "effective charging time in hours", y = "elapsed time in hours")


# Calls -------------------------------------------------------------------



