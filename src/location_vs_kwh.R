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
  filter(!is.na(latitude),!is.na(longitude), !is.na(charged_kwh))

split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(rep(target, length(index) + 1),
         start = c(1, index),
         stop = c(index -1, nchar(target)))
}

interleave <- function(v1,v2)
{
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]
}

df$latitude <- sapply(df$latitude, function(x){
  insert <- "."[order(3)]
  index <- sort(3)
  paste(interleave(split_str_by_index(x, 3), "."), collapse="")
})

df$longitude <- sapply(df$longitude, function(x){
  insert <- "."[order(2)]
  index <- sort(2)
  paste(interleave(split_str_by_index(x, 2), "."), collapse="")
})

df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)
  

#Advanced data cleaning ----
displayKwhData <- function() {
  df <- df %>%
    group_by(longitude, latitude) %>%
    summarise(total_charged = sum(charged_kwh))
  
  df$total_charged <- as.numeric(df$total_charged)
  return(df)
}

mostLeastPopular <- function() {
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
    mutate(score = total_charged/total_hours_elapsed) %>%
    arrange(desc(score))
  
  df$total_hours_elapsed <- as.numeric(df$total_hours_elapsed)
  df$total_charged <- as.numeric(df$total_charged)
  return(df)
}

table1 <- displayKwhData()
table2 <- mostLeastPopular()
table3 <- mostLeastEfficient()

chargingStationPopup <- function(id, lat, lng, cat) {
  if(cat == "kwh"){
    selectedChargingPole <- table1[table1$longitude == lng & table1$latitude == lat]
    content <- as.character(tagList(
      tags$h4("Location: %s", as.integer(lat), ", %s", as.integer(lng)),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br()
    ))
    leafletProxy("plot5") %>% 
      addPopups(lng, lat, content, layerId = id)
  }
  
  if(cat == "Popularity"){
    selectedChargingPole <- table2[table2$longitude == lng & table2$latitude == lat]
    content <- as.character(tagList(
      tags$h4("Location: %s", as.integer(lat), ", %s", as.integer(lng)),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br()
    ))
    leafletProxy("plot5") %>% 
      addPopups(lng, lat, content, layerId = id)
  }
  
  if(cat == "Efficiency"){
    selectedChargingPole <- table3[table3$longitude == lng & table3$latitude == lat]
    content <- as.character(tagList(
      tags$h4("Location: %s", as.integer(lat), ", %s", as.integer(lng)),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br()
    ))
    leafletProxy("plot5") %>% 
      addPopups(lng, lat, content, layerId = id)
  }
}
