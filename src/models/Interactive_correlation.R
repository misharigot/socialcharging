library(config)
library(readr)
library(corrplot)
library(tidyr)
library(purrr)

config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset, col_names = FALSE)

df <- cleanSecondDf(df)

# Dataframe transformation ------------------------------------------------

# filtering empty values and hours_elapsed
cleanDf <- function(df) {
  df$dayOfWeek <- weekdays(as.Date(df$start_date))
  
  df$hour <- as.numeric(format(round(df$start_date, "hours"), format = "%H"))
  
  df %>%
    filter(
      !is.na(hours_elapsed),
      hours_elapsed >= 0 & hours_elapsed <= 24,
      !is.na(start_date),
      !is.na(end_date),
      !is.na(car),
      !is.na(charged_kwh)
    ) 
}

# Classifying the time frames
classifyTf <- function(datetime) {
  datetime <- hour(datetime)
  if (datetime >= 0 & datetime < 3) {
    return(0)
  } else if (datetime >= 3 & datetime < 6) {
    return(1)
  } else if (datetime >= 6 & datetime < 9) {
    return(2)
  } else if (datetime >= 9 & datetime < 12) {
    return(3)
  } else if (datetime >= 12 & datetime < 15) {
    return(4)
  } else if (datetime >= 15 & datetime < 18) {
    return(5)
  } else if (datetime >= 18 & datetime < 21) {
    return(6)
  } else if (datetime >= 21 & datetime < 24) {
    return(7)
  }
}

# Classify session on timeframes
sessionClassificationDf <- function(cleanDf) {
  sessionClassifications <- cleanDf %>%
    mutate(kw_charge_point_speed = gsub(00, "", kw_charge_point_speed)) %>%
    mutate(
      start_tf = map(start_date, classifyTf),
      end_tf = map(end_date, classifyTf)
    ) %>%
    mutate(start_tf = as.factor(unlist(start_tf)), end_tf = as.factor(unlist(end_tf)))
  return(sessionClassifications)
}

# Convert the relevant columns to numeric type
convertDfToNumeric <- function(cleanDf) {
  df <- cleanDf
  
  numberfySmart <- function(isSmart) {
    if (isSmart == "Yes") {
      return(1)
    } else if (isSmart == "No") {
      return(0)
    }
  }
  
  numberfyCorporate <- function(isCorporate){
    if (isCorporate == "Yes") {
      return(1)
    } else if (isCorporate == "No") {
      return(0)
    }
  }
  
  # convert ev provider to numbers
  evProviderDf <- as.data.frame(unique(df$ev_provider))
  colnames(evProviderDf) <- c("ev_provider")
  evProviderDf$evNumber <- NA
  nrow(evProviderDf)
  for (i in 1:nrow(evProviderDf)) {
    evProviderDf[i, 2] <- i
  }
  
  # merge the numberfied evProviderDf with the original Df
  df <- base::merge(df, evProviderDf)
  
  # convert cars to numbers
  carDf <- as.data.frame(unique(df$car))
  colnames(carDf) <- c("car")
  carDf$carNumber <- NA
  nrow(carDf)
  for (i in 1:nrow(carDf)) {
    carDf[i, 2] <- i
  }
  
  # merge the numberfied carDf with the original Df
  df <- base::merge(df, carDf)
  
  correlationDf <- df %>%
    mutate(day_number = wday(as.Date(df$start_date))) %>%
    mutate(smart_charger = map(df$smart_charging, numberfySmart)) %>%
    mutate(smart_charger = as.numeric(unlist(smart_charger)))
  
  correlationDf$hours_elapsed <- as.numeric(correlationDf$hours_elapsed)
  correlationDf$hour <- as.numeric(correlationDf$hour)
  correlationDf$start_tf <- as.numeric(correlationDf$start_tf)
  
  # select the numeric columns
  df <- select_if(correlationDf, is.numeric)
  return(df)
}


plotCorrelation <- function(data) {
  corrplot.mixed(cor(data))
}

# Number to call for numberfied dataframe ---------------------------------

plotCorrelationplot <- function(scData) {
  plotCorrelation(convertDfToNumeric(sessionClassificationDf(cleanDf(scData))))
}
