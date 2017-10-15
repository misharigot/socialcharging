# <46 is_smart vs kWh load summed up group by station
# <compare the value of smart is yes or no >
# compare hour_elapsed, Charging per hour, effective_charging_hour and real_charging_hour

library(ggplot2)
library(config)
library(readr)

config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

#compare hour_elapsed
compareHourElapsed <- function(){
  df %>%
  group_by(smart_charging) %>% 
  filter(!is.na(hours_elapsed))
}

#compare charged_kwh per hour_elapsed
compareChargedPerHour <- function(){
  df %>% 
  group_by(smart_charging) %>% 
  mutate(real_charging_hours = charged_kwh/hours_elapsed ) %>% 
  filter(real_charging_hours < 38)
}

#compare the gap between real_charging_hours and effective_charging_hours

compareGapRealandEffective <- function(){
  df %>% 
  group_by(smart_charging) %>% 
  filter(charged_kwh/hours_elapsed <38) %>% 
  mutate(comparegap = charged_kwh/hours_elapsed - effective_charging_hours)
}


# Plot functions ----------------------------------------------------------

#plot to compare hour_elapsed

plotCompareHourElapsed <- function(){
  
  ggplot(compareHourElapsed(), aes(x = smart_charging, y = hours_elapsed)) + 
  geom_point(alpha = 0.1, aes(color = smart_charging)) +
  ggtitle("Compare Houre_Elapsed")
}

#plot to compare charged_kwh per hour_elapsed

plotCompareChargedperHour <- function(){
  ggplot(compareChargedPerHour(), aes(x = smart_charging, y = real_charging_hours)) +
  geom_point(alpha = 0.1, aes(color = smart_charging)) +
  ggtitle("Compare Charged_kwh per hour")
}

#plot to compare the gap between real_charging_hours and effective_charging_hours

plotCompareGapRealandEffiective <- function(){
  ggplot(compareGapRealandEffective(),aes(x = smart_charging, y = comparegap)) +
  geom_point(alpha = 0.1, aes(color = smart_charging)) +
  ggtitle("Compare Gap between real_charging_hours and effective_charging_hours")
}

#plot to compare effective_charging_hours and real_charging_hours
plotCompareEffectiveAndReal_charging_hours <- function(){
  ggplot(compareChargedPerHour(),aes(y=real_charging_hours, x = effective_charging_hours)) +
  geom_point(alpha = 0.4 , aes(color = smart_charging)) +
  ggtitle("Compare Effective_charging_hour and Real_charging_hours")
}


# Calls -------------------------------------------------------------------
plotCompareHourElapsed()
plotCompareChargedperHour()
plotCompareGapRealandEffiective()
plotCompareEffectiveAndReal_charging_hours()
