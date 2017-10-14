# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
library(reshape2)
config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)
df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

# Returns table with na values removed on end_date and charged_kwh
getRemovedNa <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    mutate(percentage_charging = sapply(100 / hours_elapsed * effective_charging_hours, function(x) {
      round(x, digits = 2)
    }))
}

# Returns table with smart rows
getIsSmart <- function() {
  getRemovedNa() %>%
    filter(smart_charging == 'Yes')
}

# Returns table with not-smart rows
getIsNotSmart <- function() {
  getRemovedNa() %>% 
    filter(smart_charging == 'No')
}

# AVG percentage of usage
getAvgCharigngPercentageDf <- function() {
  totalSmartCharging <- sum(getIsSmart()$percentage_charging, na.rm = T)
  avgSmartPercentage <- round(totalSmartCharging/nrow(getIsSmart()), digits = 2)
  
  totalNonSmartCharging <- sum(getIsNotSmart()$percentage_charging, na.rm = T)
  avgNonSmartPercentage <- round(totalNonSmartCharging/nrow(getIsNotSmart()), digits = 2)
  
  df <- data.frame("smart_charging" = c("Yes","No"),
                          "number_of_usage" = c(nrow(getIsSmart()), nrow(getIsNotSmart())),
                          "average_charging_percentage" = c(avgSmartPercentage,avgNonSmartPercentage))
  return(df)
}


# Plot functions ----------------------------------------------------------

plotPieChart <- function() {
  transformPlot <- ggplot(getAvgCharigngPercentageDf()) + 
    geom_bar(aes(x = smart_charging, y = number_of_usage),stat="identity") +
    labs(x = "Smart charging station", y = "number of usage") +
    ggtitle("Number of usages over charging stations") +
    coord_polar()
  
  p <- transformPlot + coord_polar("y", start=0)
  return(p)
}

# checking the amount of usages on the smart- and non smart chargers
plotBarSmart <- function() {
  reshaped <- melt(getRemovedNa(), id.vars = "smart_charging")
  p <- ggplot(getRemovedNa(), aes(x = factor(smart_charging), fill = factor(smart_charging))) + 
    geom_bar(width = 1) +
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
plotBarSmart()
plotKwhElapsedSmart()
plotEffectiveChargingHourElapsedSmart()
plotKwhElapsed()
plotEffectiveChargingHourElapsed()
plotPieChart()

multiplotHelper(plotKwhElapsedSmart(),plotKwhElapsed())
multiplotHelper(plotEffectiveChargingHourElapsedSmart(),plotEffectiveChargingHourElapsed())
