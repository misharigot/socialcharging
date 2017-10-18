# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
library(dplyr)

config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)


# Basic data cleaning ----
df <- df %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh))

split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(rep(target, length(index) + 1),
         start = c(1, index),
         stop = c(index - 1, nchar(target)))
}

interleave <- function(v1, v2) {
  ord1 <- 2 * (1:length(v1)) - 1
  ord2 <- 2 * (1:length(v2))
  c(v1, v2)[order(c(ord1, ord2))]
}

df$latitude <- sapply(df$latitude, function(x){
  insert <- "."[order(3)]
  index <- sort(3)
  paste(interleave(split_str_by_index(x, 3), "."), collapse = "")
})

df$longitude <- sapply(df$longitude, function(x){
  insert <- "."[order(2)]
  index <- sort(2)
  paste(interleave(split_str_by_index(x, 2), "."), collapse = "")
})

df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)
  

#Advanced data cleaning ----
displayKwhData <- function() {
  df <- df %>%
    group_by(longitude, latitude) %>%
    summarise(total_charged = sum(charged_kwh),
              total_sessions = n())

  df$total_sessions <- as.numeric(df$total_sessions)
  df$total_charged <- as.numeric(df$total_charged)

  return(df)
}

mostLeastEfficient <- function() {
  df <- df %>%
    filter(!is.na(hours_elapsed)) %>%
    group_by(longitude, latitude) %>%
    summarise(total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed)) %>%
    mutate(score = round((total_charged / total_hours_elapsed) * 10, digits = 2)) %>%
    arrange(desc(score))

  df$total_hours_elapsed <- as.numeric(df$total_hours_elapsed)
  df$total_charged <- as.numeric(df$total_charged)
  return(df)
}

#This method handles the Popup when clicking a circle.
chargingStationPopup <- function(id, lat, lng, cat) {
  dfKwhSession <- displayKwhData()
  dfKwhHours <- mostLeastEfficient()
  
  if (cat == "kwh") {
    selectedChargingPole <- dfKwhSession[id,]
    content <- as.character(tagList(
      tags$h4("Location: ", lat, ", ", lng),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br(),
      sprintf("Total sessions: %s", selectedChargingPole$total_sessions)
    ))
    leafletProxy("plot5") %>%  
      addPopups(lng, lat, content, layerId = id)
  }
  
  if (cat == "Popularity") {
    selectedChargingPole <- dfKwhSession[id,]
    content <- as.character(tagList(
      tags$h4("Location: ", lat, ", ", lng),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br(),
      sprintf("Total sessions: %s", selectedChargingPole$total_sessions)
    ))
    leafletProxy("plot5") %>% 
      addPopups(lng, lat, content, layerId = id)
  }
  
  if (cat == "Efficiency") {
    selectedChargingPole <- dfKwhHours[id,]
    content <- as.character(tagList(
      tags$h4("Location: ", lat, ", ", lng),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br(),
      sprintf("Total elapsed hours: %s", selectedChargingPole$total_hours_elapsed), tags$br(),
      sprintf("Score: %s", selectedChargingPole$score)
    ))
    leafletProxy("plot5") %>%  
      addPopups(lng, lat, content, layerId = id)
  }
}
