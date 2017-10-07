# Plot charged_kwh against hours_elapsed
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(config)
config <- config::get()

df <- read_csv2(config$scDataset)

df <- df %>% 
  rename(charged_kwh = `charged_kwh,`) %>%
  filter(end_date != "NULL", !is.na(charged_kwh), charged_kwh != "Unknown") %>%
  mutate(start_date = dmy_hm(start_date), 
         end_date = dmy_hm(end_date), 
         charged_kwh = as.numeric(charged_kwh),
         hours_elapsed = sapply(end_date - start_date, function(x) {
           round(x/3600, 2)
         }))

p <- ggplot(df, aes(y = charged_kwh, x = hours_elapsed)) + geom_point(alpha = 0.3) + geom_smooth()
p + labs(x = "session time in hours", y = "kWh charged")
