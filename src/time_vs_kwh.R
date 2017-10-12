# Plot charged_kwh against hours_elapsed per station
library(readr)
library(ggplot2)
library(config)
config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

getDfHoursElapsed <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    mutate(hours_elapsed = sapply(end_date - start_date, function(x) {
      round(x/3600, 2)
    }))
}

# Plot functions ----------------------------------------------------------

plotTimeKwh <- function() {
  p <- ggplot(GetDfHoursElapsed(), aes(y = charged_kwh, x = hours_elapsed)) + 
    geom_point(alpha = 0.3) + 
    geom_smooth() + 
    labs(x = "session time in hours", y = "kWh charged")
  return(p)
}

# Calls -------------------------------------------------------------------

plotTimeKwh()
