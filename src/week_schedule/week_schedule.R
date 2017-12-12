library(timevis)
library(config)
library(readr)
library(timevis)
library(dplyr)

config <- config::get(file = "config.yml")
source(config$baseClean)

# df <- read_csv2(config$scDataset, col_names = FALSE)
# df <- cleanDataframe(df)

# make df for selected user and selected date 
selectData <- function(df, user, dates){
  df %>%
    filter(user_id == user & start_date >= dates[1] & end_date <= dates[2]) %>%
    mutate(start_date = as.character(start_date), end_date = as.character(end_date)) %>% 
    select(start_date, end_date, charged_kwh)
}

# by using above df it makes data.frame for timevis
makeDf <- function(selectData, dates){
  # this if is for when there is no data in selected date
  if (nrow(selectData) == 0) {
    data <- data.frame(
      content = "NO DATA",
      start = dates[1],
      end = dates[2]
    )
    return(data)
  }else{
    data <- data.frame(
      id = 1:nrow(selectData),
      content = paste0(selectData$charged_kwh, " kwh"),
      start = selectData$start_date,
      end = selectData$end_date
    )
    return(data)  
  }
}

showTimevis <- function(scData, user, dates){
  makeDf(selectData(scData, user, dates), dates)
}
