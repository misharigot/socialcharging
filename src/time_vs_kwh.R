# Plot charged_kwh against hours_elapsed per station
library(readr)
library(ggplot2)
library(config)
config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

getDfHoursElapsed <- function() {
  df %>%
    filter(!is.na(end_date), !is.na(charged_kwh)) %>%
    mutate(hours_elapsed = sapply(end_date - start_date, function(x) {
      round(x / 3600, 2)
    }))
}

# Plot functions ----------------------------------------------------------

plotTimeKwh <- function() {
  p <- ggplot(getDfHoursElapsed(), aes(y = charged_kwh, x = hours_elapsed)) +
    geom_jitter(colour = alpha("black", 0.2)) +
    geom_smooth() +
    labs(title = "Charging sessions",
         subtitle = "kWh charged and time elapsed",
         x = "session time in hours",
         y = "kWh charged")

  return(p)
}

# Calls -------------------------------------------------------------------

plotTimeKwh()
