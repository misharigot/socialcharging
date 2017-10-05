library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(config)
config <- config::get()

data <- read_csv(config$scDataset)

data$end_datetime <- as.POSIXct(strptime(data$end_datetime, "%Y-%m-%d %H:%M:%S"))
data$time_elapsed <- sapply(data$end_datetime - data$start_datetime, function(x) {
  round(x/60, 2)
})

# plot charged_kwh against time_elapsed in hours
p <- ggplot(data, aes(y = charged_kwh, x = time_elapsed)) + geom_smooth(alpha = 0.5)
p + labs(x = "time elapsed in hours", y = "kWh charged")
