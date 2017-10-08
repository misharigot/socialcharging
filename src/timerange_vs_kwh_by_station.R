# Timerange vs kWh grouped by charging station
library(config)
config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

a <- df %>%
  group_by(address) %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  summarise(sum_kwh = sum(charged_kwh))

b <- tapply(df, month(start_date), sum)