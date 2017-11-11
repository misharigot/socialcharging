# User classification model
library(readr)
library(ggplot2)
library(config)
config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

sc_data <- df %>%
  # select(-car, -ev_provider, -corporate, -smart_charging, -evse_id, -address) %>%
  # select(charged_kwh, hours_elapsed) %>%
  na.omit(df) %>%
  group_by(user_id) %>%
  summarise(charged_kwh = mean(charged_kwh), hours_elapsed = mean(hours_elapsed))

sc_data <- sc_data %>% select(2:3)

sc_km <- kmeans(sc_data, centers = 8, nstart = 20)
plot(y = sc_data$charged_kwh, x = sc_data$hours_elapsed, col = sc_km$cluster)