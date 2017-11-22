# This script contains multiple functions to help the map creation and legend creation
library(shiny)
library(ggplot2)
library(leaflet)

# Functions --------------------------------------------------------------------------------------------------------------
# Create a pallete for the circles and the legend
createPallete <- function(mapData, input = "occ_perc") {
  if (length(input) == 0) {return()}
  if (input == "charged_kwh") {
    values <- mapData$total_charged
    pal <- createColors(values)
  }
  
  if (input == "total_hours_elapsed") {
    values <- mapData$total_hours_elapsed
    pal <- createColors(values)
  }
  
  if (input == "total_sessions") {
    values <- mapData$total_sessions
    pal <- createColors(values)
  }
  
  if (input == "occ_perc") {
    values <- mapData$popularity_score / max(mapData$popularity_score) * 300
    pal <- createColors(values)
  }
  
  if (input == "eff_perc") {
    values <- mapData$efficiency_score / max(mapData$efficiency_score) * 300
    pal <- createColors(values)
  }
  
  if (input == "users_station") {
    values <- mapData$total_users / max(mapData$total_users) * 300
    pal <- createColors(values)
  }
  return(pal)
}

# Returns color values
createColors <- function(values) {
  if (length(unique(values)) > 1) { # can only make bins of more than 1 value
    colorBin("plasma", values, 5, pretty = FALSE)
  } else {
    colorNumeric(palette = "plasma", domain = values)
  }
}

# Create the color of the circles
createCircleColor <- function(mapData, input = "charged_kwh", pal) {
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
  
  if (input == "occ_perc") {
    color <- pal(mapData$popularity_score / max(mapData$popularity_score) * 300)
  }
  
  if (input == "eff_perc") {
    color <- pal(mapData$efficiency_score / max(mapData$efficiency_score) * 300)
    
  }
  
  if (input == "users_station") {
    color <- pal(mapData$total_users / max(mapData$total_users) * 300)
  }
  return(color)
}

# Create the radius of the circles
createCircleSize <- function(mapData, input = "occ_perc") {
  logMultiplier <- 30
  if (length(input) == 0) {return()}
  
  if (input == "charged_kwh") {
    radius <- log(mapData$total_charged) * logMultiplier
  }
  
  if (input == "total_hours_elapsed") {
    radius <- log(mapData$total_hours_elapsed) * logMultiplier
  }
  
  if (input == "total_sessions") {
    radius <- log(mapData$total_sessions) * logMultiplier
  }
  
  if (input == "occ_perc") {
    radius <- log(mapData$popularity_score / max(mapData$popularity_score) * 300) * logMultiplier
  }
  
  if (input == "eff_perc") {
    radius <- log(mapData$efficiency_score / max(mapData$efficiency_score) * 300) * logMultiplier
  }
  
  if (input == "users_station") {
    radius <- log(mapData$total_users / max(mapData$total_users) * 300) * logMultiplier
  }
  
  return(radius)
}

# Create the values for the legend
createLegendValues <- function(mapData, input = "charged_kwh") {
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
  
  if (input == "occ_perc") {
    value <- mapData$popularity_score / max(mapData$popularity_score) * 300
  }
  
  if (input == "eff_perc") {
    value <- mapData$efficiency_score / max(mapData$efficiency_score) * 300
  }
  
  if (input == "users_station") {
    value <- mapData$total_users / max(mapData$total_users) * 300
  }
  return(value)
}

# Create the legend title
createLegendTitle <- function(input = "charged_kwh") {
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
  
  if (input == "occ_perc") {
    title <- "Percentage of time occupied"
  }
  
  if (input == "eff_perc") {
    title <- "Efficiency percentage"
  }
  
  if (input == "users_station") {
    title <- "Amount of users per station"
  }
  return(title)
}
