# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

df <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh))

CreateBarPlotSmartKwh <- function() {
  # checking the amount of usages on the smart- and non smart chargers
  barplot <- ggplot(data=df,aes(x=smart_charging)) + geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=-1)
  barplot + labs(x = "smart charging")
  return(barplot)
}

# subsetting the smart- and non smart chargers
df_is_smart <- filter(df, df$smart_charging == 'Yes')
df_not_smart <- filter(df, df$smart_charging == 'No')

CreatePlotSmartKwh1 <- function() {
# plot 1 
  plotSmartChargers1 <- ggplot(df_is_smart, aes(y = charged_kwh, x = hours_elapsed)) + geom_point(alpha = 0.3) + geom_smooth()
  plotSmartChargers1 + labs(x = "session time in hours", y = "kWh charged")
  return(plotSmartChargers1)
}

CreatePlotSmartKwh2 <- function() {
  # plot 1.1
  plotSmartChargers2 <- ggplot(df_is_smart, aes(y = effective_charging_hours, x = hours_elapsed)) + geom_point(alpha = 0.3)
  plotSmartChargers2 + labs(x = "effective charging time in hours", y = "elapsed time in hours")
  return(plotSmartChargers2)
}

CreatePlotSmartKwh3 <- function() {
  # plot 2
  plotNonSmartChargers1 <- ggplot(df_not_smart, aes(y = charged_kwh, x = hours_elapsed)) + geom_point(alpha = 0.3) + geom_smooth()
  plotNonSmartChargers1 + labs(x = "session time in hours", y = "kWh charged")
  return(plotNonSmartChargers1)
}

CreatePlotSmartKwh4 <- function() {
  # plot 2.1
  plotNonSmartChargers2 <-  ggplot(df_not_smart, aes(y = effective_charging_hours, x = hours_elapsed)) + geom_point(alpha = 0.3) 
  plotNonSmartChargers2 + labs(x = "effective charging time in hours", y = "elapsed time in hours")
  return(plotNonSmartChargers2)
}
