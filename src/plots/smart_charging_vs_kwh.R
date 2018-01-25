# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
config <- config::get(file = "config.yml")
source(config$baseClean)
source("src/helpers/multiplot_helper.R")

cleanScData <- function(scData) {
  scData %>%
    filter(!is.na(end_date), !is.na(charged_kwh), hours_elapsed > 0) %>%
    mutate(percentage_charging = sapply(100 / hours_elapsed * effective_charging_hours, function(x) {
      round(x, digits = 2)
    }))
}

# Table functions ---------------------------------------------------------

# Returns table with smart rows
getIsSmart <- function(scData) {
  scData %>%
    filter(smart_charging == "Yes")
}

# Returns table with not-smart rows
getIsNotSmart <- function(scData) {
  scData %>%
    filter(smart_charging == "No")
}

# Creates a simple dataframe with AVG percentage of usage
getAvgChargingPercentageDf <- function(scData) {
  avgSmartPercentage <- round(mean(getIsSmart(scData)$percentage_charging, na.rm = T), digits = 2)
  avgSmartChargeTime <- round(mean(getIsSmart(scData)$hours_elapsed, na.rm = T), digits = 2)
  avgNonSmartChargePercentage <- round(mean(getIsNotSmart(scData)$percentage_charging, na.rm = T), digits = 2)
  avgNonSmartChargeTime <- round(mean(getIsNotSmart(scData)$hours_elapsed, na.rm = T), digits = 2)
  avgSmartEffTime <- round(mean(getIsSmart(scData)$effective_charging_hours, na.rm = T), digits = 2)
  avgNonSmartEffTime <- round(mean(getIsNotSmart(scData)$effective_charging_hours, na.rm = T), digits = 2)
  tempDf <- data.frame( "smart_charging" = c("Yes", "No"),
                        "number_of_usage" = c(nrow(getIsSmart(scData)), nrow(getIsNotSmart(scData))),
                        "average_charging_percentage" = c(avgSmartPercentage, avgNonSmartChargePercentage),
                        "average_charging_time" = c(avgSmartChargeTime, avgNonSmartChargeTime),
                        "average_eff_charge_time" = c(avgSmartEffTime, avgNonSmartEffTime))
  return(tempDf)
}

# Plot functions ----------------------------------------------------------

# Simple bar chart displaying smart- and non smart usages
plotBarSmart <- function(scData) {
  scData <- cleanScData(scData)
  p <- ggplot(scData, aes(x = factor(1), fill = factor(smart_charging))) +
    geom_bar(width = 0.2) +
    geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..) * 100),'%')),
              position = position_stack( vjust = 0.5 )) +
    theme_void() + scale_fill_manual(values=c("#B22222", "#66bb6a")) +
    guides( fill = guide_legend(title = "Smart charging")) +
    theme( legend.justification = c(1, 0), legend.position = c(1, 0), legend.text = element_text(size = 18),
           legend.title = element_text(size = 20)) +
    ggtitle("Started sessions of smart and non smart (barchart)") +
    theme(plot.title = element_text( hjust = 0.5, size = 20))
  return(p)
}

# Returns a bar plot displaying the avg charge time
plotChargeTime <- function(scData) {
  scData <- cleanScData(scData)
  p <- ggplot(getAvgChargingPercentageDf(scData), aes(x = smart_charging, y = average_charging_time,
                                                fill = factor(smart_charging))) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(data = getAvgChargingPercentageDf(scData), aes(label = paste0(average_charging_time,'h')),
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = "Average charging time in hours") +
    ggtitle("Average chargetime") +
    theme_light() + scale_fill_manual(values=c("#B22222", "#66bb6a")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title = element_text(size = 20),
          axis.text = element_text(size = 18)) +
    guides(fill = FALSE)
  return(p)
}

#  Returns a bar plot displaying the avg charging percentage
plotChargePercentage <- function(scData) {
  scData <- cleanScData(scData)
  p <- ggplot(getAvgChargingPercentageDf(scData), aes(x = smart_charging, y = average_charging_percentage,
                                                fill = factor(smart_charging))) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(data = getAvgChargingPercentageDf(scData), aes(label = paste0(average_charging_percentage,'%')),
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = "Effective % of total time car charging") +
    ggtitle("Effective time battery charged ") +
    theme_light() + scale_fill_manual(values=c("#B22222", "#66bb6a")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title = element_text(size = 20),
          axis.text = element_text(size = 18)) +
    guides(fill = FALSE)
  return(p)
}

# Plots multiple plots side by side
plotMultiple <- function(scData) {
  return(multiplotHelper(plotChargeTime(scData), plotChargePercentage(scData), plotBarSmart(scData), cols = 2))
}

# Calls -------------------------------------------------------------------

# plotMultiple()
