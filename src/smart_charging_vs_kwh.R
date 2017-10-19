# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)
df <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(percentage_charging = sapply(100 / hours_elapsed * effective_charging_hours, function(x) {
    round(x, digits = 2)
  }))

# Table functions ---------------------------------------------------------

# Returns table with smart rows
getIsSmart <- function() {
  df %>%
    filter(smart_charging == "Yes")
}

# Returns table with not-smart rows
getIsNotSmart <- function() {
  df %>%
    filter(smart_charging == "No")
}

# Creates a simple dataframe with AVG percentage of usage
getAvgChargingPercentageDf <- function() {
  avgSmartPercentage <- round(mean(getIsSmart()$percentage_charging, na.rm = T), digits = 2)
  avgSmartChargeTime <- round(mean(getIsSmart()$hours_elapsed, na.rm = T), digits = 2)
  avgNonSmartChargePercentage <- round(mean(getIsNotSmart()$percentage_charging, na.rm = T), digits = 2)
  avgNonSmartChargeTime <- round(mean(getIsNotSmart()$hours_elapsed, na.rm = T), digits = 2)
  avgSmartEffTime <- round(mean(getIsSmart()$effective_charging_hours, na.rm = T), digits = 2)
  avgNonSmartEffTime <- round(mean(getIsNotSmart()$effective_charging_hours, na.rm = T), digits = 2)
  tempDf <- data.frame( "smart_charging" = c("Yes", "No"),
                        "number_of_usage" = c(nrow(getIsSmart()), nrow( getIsNotSmart())),
                        "average_charging_percentage" = c(avgSmartPercentage, avgNonSmartChargePercentage),
                        "average_charging_time" = c(avgSmartChargeTime, avgNonSmartChargeTime),
                        "average_eff_charge_time" = c(avgSmartEffTime, avgNonSmartEffTime))
  return(tempDf)
}
a <- getAvgChargingPercentageDf()
# Plot functions ----------------------------------------------------------

# Simple bar chart displaying smart- and non smart usages
plotBarSmart <- function() {
  p <- ggplot(df, aes(x = factor(1), fill = factor(smart_charging))) +
    geom_bar(width = 0.2) +
    geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..) * 100),'%')),
              position = position_stack( vjust = 0.5 )) +
    theme_void() +
    guides( fill = guide_legend(title = "Smart charging")) +
    theme( legend.justification = c(1, 0), legend.position = c(1, 0)) +
    ggtitle("Started sessions of smart and non smart (barchart)") +
    theme(plot.title = element_text( hjust = 0.5 ))
  return(p)
}

# Simple pie chart displaying smart- and non smart usages
# width of 1 creates a pie chart, anything less creates a donut chart
plotPieChart <- function() {
  p <- ggplot(df, aes(x = factor(1), fill = factor(smart_charging))) +
    geom_bar(width = 0.3) +
    labs(y = "smart charging") +
    geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5)) +
    coord_polar("y", start = 0, direction = -1) +
    theme_void() +
    ggtitle("Started sessions of smart and non smart (piechart)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill = FALSE)
  return(p)
}

# plotChargeTime a bar plot displaying the avg charge time
plotChargeTime <- function() {
  p <- ggplot(getAvgChargingPercentageDf(), aes(x = smart_charging, y = average_charging_time,
                                                fill = factor(smart_charging))) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(data = getAvgChargingPercentageDf(), aes(label = paste0(average_charging_time,'h')),
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = "Average charging time in hours") +
    ggtitle("Average chargetime") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill = FALSE)
  return(p)
}

# plotChargePercentage a bar plot displaying the avg charging percentage
plotChargePercentage <- function() {
  p <- ggplot(getAvgChargingPercentageDf(), aes(x = smart_charging, y = average_charging_percentage,
                                                fill = factor(smart_charging))) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(data = getAvgChargingPercentageDf(), aes(label = paste0(average_charging_percentage,'%')),
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = "Effective % of total time car charging") +
    ggtitle("Effective time battery charged ") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill = FALSE)
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
plotEffectiveChargingHoursElapsedSmart <- function() {
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
plotEffectiveChargingHoursElapsed <- function() {
  p <- ggplot(getIsNotSmart(), aes(x = hours_elapsed, y = effective_charging_hours)) +
    geom_point(alpha = 0.3) +
    geom_smooth(alpha = 0.2, size = 1) +
    labs(x = "elapsed time in hours", y = "effective charging time in hours") +
    #xlim(0,40) +
    ggtitle("Effective charging time vs hours elapsed for non-smart chargers")
  return(p)
}

# Plots multiple plots side by side
plotMultiple <- function() {
  return(multiplotHelper(plotChargeTime(), plotChargePercentage(), plotBarSmart(), cols = 2))
}

# Calls -------------------------------------------------------------------
plotKwhElapsedSmart()
plotEffectiveChargingHoursElapsedSmart()
plotKwhElapsed()
plotEffectiveChargingHoursElapsed()

plotMultiple()
