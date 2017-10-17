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
  labs(x = "Smart Charing", y = "Hours Elapsed") +
  ggtitle("Compare Houre_Elapsed")
}

#plot to compare charged_kwh per hour_elapsed

plotCompareChargedperHour <- function(){
  ggplot(compareChargedPerHour(), aes(x = smart_charging, y = real_charging_hours)) +
  geom_point(alpha = 0.1, aes(color = smart_charging)) +
  labs(x = "Smart Charging", y = "Real Charging Per Hour") +
  ggtitle("Compare Charged_kwh Per Hour")
}

#plot to compare the gap between real_charging_hours and effective_charging_hours

plotCompareGapRealandEffiective <- function(){
  ggplot(compareGapRealandEffective(),aes(x = smart_charging, y = comparegap)) +
  geom_point(alpha = 0.1, aes(color = smart_charging)) +
  labs(x = "Smart Charging", y = "Gap between Real and Effective Charging per hours") +
  ggtitle("Compare Gap between Effective and Real Charging Per Hours")
}

#plot to compare effective_charging_hours and real_charging_hours
plotCompareEffectiveAndReal_charging_hours <- function(){
  ggplot(compareChargedPerHour(),aes(y=real_charging_hours, x = effective_charging_hours)) +
  geom_point(alpha = 0.4 , aes(color = smart_charging)) +
  labs(x = "Effective Charging per Hour", y = "Real Charging per Hour") +
  ggtitle("Compare Effective and Real Charging  Per hours")
}


# Calls -------------------------------------------------------------------
plotCompareHourElapsed()
plotCompareChargedperHour()
plotCompareGapRealandEffiective()
plotCompareEffectiveAndReal_charging_hours()
