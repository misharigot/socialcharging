library(ggplot2)
library(config)
library(readr)
library(dplyr)
config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Per user total amount charged kwh
kwhPerUser <- df %>%
  group_by(user_id) %>%
  filter(!is.na(charged_kwh)) %>%
  summarise(total_charged = sum(charged_kwh), 
            total_hours = sum(hours_elapsed), 
            total_effective_hours = sum(effective_charging_hours))

#Plotje
plot <- ggplot(kwhPerUser, aes(y = total_charged, x = total_hours)) + geom_point(alpha = 0.3) + geom_smooth()
plot + labs(x = "Total hours", y = "Total kWh charged")
