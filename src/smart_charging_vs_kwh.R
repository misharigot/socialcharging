# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
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
    filter(smart_charging == "Yes")
}

# Returns table with not-smart rows
getIsNotSmart <- function() {

  getRemovedNa() %>% 
    filter(smart_charging == 'No')
}

# Creates a simple dataframe with AVG percentage of usage
getAvgChargingPercentageDf <- function() {
  avgSmartPercentage <- round(sum(getIsSmart()$percentage_charging, na.rm = T)
                              /nrow(getIsSmart()), digits = 2)
  avgSmartChargeTime <- round(sum(getIsSmart()$hours_elapsed, na.rm = T)/nrow(getIsSmart()), digits = 2)
  
  avgNonSmartChargePercentage <- round(sum(getIsNotSmart()$percentage_charging, na.rm = T)
                                       /nrow(getIsNotSmart()), digits = 2)
  avgNonSmartChargeTime <- round(sum(getIsNotSmart()$hours_elapsed, na.rm = T)/nrow(getIsNotSmart()), digits = 2)
  
  tempDf <- data.frame("smart_charging" = c("Yes","No"),
                          "number_of_usage" = c(nrow(getIsSmart()), nrow(getIsNotSmart())),
                          "average_charging_percentage" = c(avgSmartPercentage,avgNonSmartChargePercentage),
                          "average_charging_time" = c(avgSmartChargeTime, avgNonSmartChargeTime))
  return(tempDf)
}

# Plot functions ----------------------------------------------------------

# Simple bar chart displaying smart- and non smart usages
plotBarSmart <- function() {
  p <- ggplot(getRemovedNa(), aes(x = factor(1), fill = factor(smart_charging))) + 
    geom_bar(width = 0.3) + 
    geom_text(stat = 'count' ,aes(label = ..count..), position = position_stack(vjust = 0.5)) +
    theme_void() +
    guides(fill=guide_legend(title="Smart charging"))
  return(p)
}

# Simple pie chart displaying smart- and non smart usages
# width of 1 creates a pie chart, anything less creates a donut chart
plotPieChart <- function() {
  p <- ggplot(getRemovedNa(), aes(x = factor(1), fill = factor(smart_charging))) + 
    geom_bar(width = 0.3) +
    labs(y = "smart charging") +
    geom_text(stat = 'count' ,aes(label = ..count..), position = position_stack(vjust = 0.5)) +
    coord_polar("y", start = 0, direction = -1) +
    theme_void() +
    guides(fill= FALSE)
  return(p)
}

# plotChargeTime a bar plot displaying the avg charge time
plotChargeTime <- function() {
  p <- ggplot(getAvgChargingPercentageDf(), aes(x = smart_charging, y = average_charging_time,
                                                fill = factor(smart_charging))) + 
    geom_bar(stat="identity", width = 1) +
    geom_text(data = getAvgChargingPercentageDf() ,aes(label = average_charging_time), 
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL , y = "Average charging time in hours") +
    guides(fill= FALSE)
  return(p)
}

# plotChargePercentage a bar plot displaying the avg charging percentage
plotChargePercentage <- function() {
  p <- ggplot(getAvgChargingPercentageDf(), aes(x = smart_charging, y = average_charging_percentage,
                                                fill = factor(smart_charging))) + 
    geom_bar(stat="identity", width = 1) +
    geom_text(data = getAvgChargingPercentageDf() ,aes(label = average_charging_percentage), 
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL , y = "Average % battery charged") +
    guides(fill= FALSE)
  return(p)
}

# IsSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotKwhElapsedSmart <- function() {
  p <-  ggplot(getIsSmart(), aes(x = hours_elapsed, y = charged_kwh)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "session time in hours", y = "kWh charged") +
    ggtitle("Charging time vs hours elapsed for smart chargers") 
  return(p)
}

# IsSmart scatterplot displaying a possible relation between the effective charging time and session time
plotEffectiveChargingHourElapsedSmart <- function() {
  p <- ggplot(getIsSmart(), aes(x = hours_elapsed, y = effective_charging_hours)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "elapsed time in hours", y = "effective charging time in hours") +
    #xlim(0,40) +
    ggtitle("Effective charging time vs hours elapsed for smart chargers")
  return(p)
}

# NotSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotKwhElapsed <- function() {
  p <- ggplot(getIsNotSmart(), aes(x = hours_elapsed, y = charged_kwh)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "session time in hours", y = "kWh charged") +
    ggtitle("Charging time vs hours elapsed for non-smart chargers")
  return(p)
}

# NotSmart scatterplot displaying a possible relation between the amount of charged kwh and session time
plotEffectiveChargingHourElapsed <- function() {
  p <- ggplot(getIsNotSmart(), aes(x = hours_elapsed, y = effective_charging_hours)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "elapsed time in hours", y = "effective charging time in hours") +
    #xlim(0,40) +
    ggtitle("Effective charging time vs hours elapsed for non-smart chargers")
  return(p)
}

# Calls -------------------------------------------------------------------

plotKwhElapsedSmart()
plotEffectiveChargingHourElapsedSmart()
plotKwhElapsed()
plotEffectiveChargingHourElapsed()

multiplotHelper(plotBarSmart(),plotPieChart(),plotChargeTime(),plotChargePercentage(), cols =2)
