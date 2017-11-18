# This script handles multiple functions to help the map creation and legend creation
library(shiny)
library(ggplot2)
library(leaflet)

# Functions --------------------------------------------------------------------------------------------------------------
# Create a pallet for the circles and the legend
createPallet <- function(input, mapData) {
  if (length(input) == 0) {return()}
  if (input == "charged_kwh") {
    pal <- colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
  }
  
  if (input == "total_hours_elapsed") {
    pal <- colorBin("plasma", mapData$total_hours_elapsed, 5, pretty = FALSE)
  }
  
  if (input == "total_sessions") {
    pal <- colorBin("plasma", mapData$total_sessions, 5, pretty = FALSE)
  }
  return(pal)
}

# Create the color of the circles
createCircleColor <- function(input, mapData, pal) {
  if (length(input) == 0) {return()}
  if (input == "charged_kwh") {
    color <- pal(mapData$total_charged)
  }
  
  if (input == "total_hours_elapsed") {
    color <- pal(mapData$total_hours_elapsed)
  }
  
  if (input == "total_sessions") {
    color <- pal(mapData$total_sessions)
  }
  return(color)
}

# Create the radius of the circles
createCircleSize <- function(input, mapData) {
  if (length(input) == 0) {return()}
  if (input == "occ_perc") {
    radius <- mapData$popularity_score / max(mapData$popularity_score) * 300
  }
  
  if (input == "eff_perc") {
    radius <- mapData$efficiency_score / max(mapData$efficiency_score) * 300
  }
  
  if (input == "users_station") {
    radius <- mapData$total_users / max(mapData$total_users) * 300
  }
  return(radius)
}

# Create the values for the legend
createLegendValues <- function(input, mapData) {
  if (length(input) == 0) {return()}
  if (input == "charged_kwh") {
    value <- mapData$total_charged
  }
  
  if (input == "total_hours_elapsed") {
    value <- mapData$total_hours_elapsed
  }
  
  if (input == "total_sessions") {
    value <- mapData$total_sessions
  }
  return(value)
}

# Create the legend title
createLegendTitle <- function(input) {
  if (length(input) == 0) {return()}
  if (input == "charged_kwh") {
    title <- "Charged kWh"
  }
  
  if (input == "total_hours_elapsed") {
    title <- "Hours elapsed"
  }
  
  if (input == "total_sessions") {
    title <- "Amount of sessions"
  }
  return(title)
}
