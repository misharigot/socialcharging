# User classification model
library(readr)
library(ggplot2)
library(config)
library(purrr)
library(tidyr)
library(clValid)
library(plotly)
source("src/helpers/coordinate_helper.R")

config <- config::get(file = "config.yml")
source(config$baseClean)

set.seed(100)

# Data cleaning --------------------------------------------------------------------------------------
cleanDataFrame <- function(df) {
  df <- df %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed))
  
  df$latitude <- sapply(df$latitude, formatCoordinate, "latitude")
  df$longitude <- sapply(df$longitude, formatCoordinate, "longitude")
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  
  df <- df %>%
    group_by(longitude, latitude) %>%
    summarise(address = first(address),
              outlets = first(outlets),
              total_sessions = n(),
              total_users = n_distinct(user_id),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed))
  
  df$total_sessions <- as.numeric(df$total_sessions)
  df$total_charged <- as.numeric(df$total_charged)
  
  return(df)
}

createClusterDataFrame <- function(scData) {
  cleanedDf <- cleanDataFrame(scData)
  clusterDf <- cleanedDf %>%
    ungroup() %>%
    select(total_charged, total_hours_elapsed, total_sessions)
  
  rownames(clusterDf) <- paste(cleanedDf$longitude, cleanedDf$latitude, sep = ", ")
  
  return(clusterDf)
}

# Ploting ---------------------------------------------------------------------------------------------------
createStationClusterPlot <- function(scData) {
  
  clusterDf <- createClusterDataFrame(scData)
  
  charging_km <- kmeans(clusterDf, 4, nstart = 20)
  
  p <- plot_ly(clusterDf, x = ~total_hours_elapsed, y = ~total_charged,
               z = ~total_sessions, color = charging_km$cluster, showscale = TRUE,
               hoverinfo = 'text',
               text = ~paste('</br> Hours elapsed: ', total_hours_elapsed,
                             '</br> Charged kWh: ', total_charged,
                             '</br> Sessions: ', total_sessions)) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = 'total hours elapsed'),
                        yaxis = list(title = 'total charged kwh'),
                        zaxis = list(title = 'total sessions')))
  return(p)
}

# Performance measures ----------------------------------------------------------------------------------------
##Dunn's Index
calculateDunnIndex <- function() {
  dunn_km <- dunn(clusters = charging_km$cluster, Data = clusterDf)
  return(dunn_km)
}

##Scree Plot
createScreePlot <- function() {
  
  ratio_ss <- rep(0, 10)
  
  for (k in 1:10) {
    charging_km_test <- kmeans(clusterDf, k, nstart = 20)
    
    dunn_km_test <- dunn(clusters = charging_km_test$cluster, Data = clusterDf)
    
    print(k)
    ratio_ss[k] <- charging_km_test$tot.withinss / charging_km_test$totss
  }
  
  plot(ratio_ss, type = "b", xlab = "k")
}

# createStationClusterPlot()
