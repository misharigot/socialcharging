library(ggplot2)
library(config)
library(readr)
library(lubridate)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)

df <- read_csv2(config$scBigDataset, col_names = FALSE)
dfHeaders <- read_csv2(config$scDataset)
colnames(df) <- colnames(dfHeaders)
rm(dfHeaders)

df <- cleanSecondDataframe(df)
df <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh)) %>%
  mutate(percentage_charging = sapply(100 / hours_elapsed * effective_charging_hours, function(x) {
    round(x, digits = 2)
  }))

minDate <- ymd_hms("2017-05-05 00:00:00")
maxDate <- ymd_hms("2017-10-30 00:00:00")
emptyMaxDate <- ymd_hms("")

if(is.null(emptyMaxDate) || is.na(emptyMaxDate)){
  test <- subset(df, start_date > minDate)
} else {
  test <- subset(df, start_date > minDate & start_date < emptyMaxDate)
}

# need to test this function
constructDataframe <- function(inputDataFrame){
  if(missing(inputDataFrame)){
    return("No dataframe supplied")
  } else {
    countDf <- inputDataFrame %>%
      group_by(user_id) %>%
      summarise(car_count = n())
    
    avgChargedKwhDf <- inputDataFrame %>%
      group_by(user_id) %>%
      summarise(average_charged_kwh = mean(charged_kwh))
    
    totalChargingHoursDf <- inputDataFrame %>%
      group_by(user_id) %>%
      summarise(average_charging_hours = mean(hours_elapsed))
    
    effChargingHoursDf <- inputDataFrame %>%
      group_by(user_id) %>%
      summarise(average_effective_charging_hours = mean(effective_charging_hours))
    
    # needs to merge the different dataframes
    totalDf <- merge()
  }
}


test <- lm()