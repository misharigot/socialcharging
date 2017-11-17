library(shiny)
library(ggplot2)
library(leaflet)
source("src/helpers/coordinate_helper.R")

# UI --------------------------------------------------------------------------------------------------------------

regressionMapModuleUI <- function(id) {
  ns <- NS(id)
  div(class = "outer",
      tags$head(
        # Include our custom CSS
        includeCSS("src/map/styles.css"),
        includeScript("src/map/gomap.js")
      ),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput(ns("regressionMap"), width = "100%", height = "100%"),
      absolutePanel(id = "controls",
                    class = "panel panel-default",
                    fixed = TRUE,
                    draggable = FALSE,
                    top = 60,
                    left = "auto",
                    right = 20,
                    bottom = "auto",
                    width = 330,
                    height = "auto",
                    h2("Filter controls"),
                    h5("These selections determine the prediction model and attributes for it:"),
                    selectInput("prediction",
                                "Prediction based on",
                                c(
                                  "Profile Regression" = "profiling"
                                )
                    ),
                    uiOutput("user_selection"),
                    uiOutput("session_selection")
      )
  )
}

# Server ----------------------------------------------------------------------------------------------------------

regressionMapModule <- function(input, output, session, data) {
  # Converts raw SC data into data prepped for the leaflet map
  mapData <- reactive({
    data
  })
  
  # The rendered leaflet map
  output$map <- renderLeaflet({
    handleDefaultRegressionMapCreation(mapData = mapData())
  })
  
  # Updates map when category input changes
  observeEvent(input$sessions, {
    handleRegressionMapCreation(mapData = mapData())
  })
  
  # Updates map with popup when a node is clicked
  observeEvent(input$map_shape_click, {
    handleRegressionPopupCreation(input$map_shape_click, mapData = mapData())
  })
  
}

# Functions -------------------------------------------------------------------------------------------------------

# Returns a data set prepared for the leaflet map, based on SC data
getMapData <- function(scData) {
  mapDf <- scData %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed))
  
  mapDf$latitude <- sapply(mapDf$latitude, formatCoordinate, "latitude")
  mapDf$longitude <- sapply(mapDf$longitude, formatCoordinate, "longitude")
  mapDf$latitude <- as.numeric(mapDf$latitude)
  mapDf$longitude <- as.numeric(mapDf$longitude)
  
  totalHours <- interval(min(mapDf$start_date), max(mapDf$end_date)) / 3600
  
  mapDf <- mapDf %>%
    group_by(longitude, latitude) %>%
    summarise(address = first(address),
              outlets = first(outlets),
              total_sessions = n(),
              total_users = n_distinct(user_id),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed),
              total_effective_charging = sum(effective_charging_hours)) %>%
    mutate(efficiency_score = round((total_effective_charging / total_hours_elapsed) * 100 + 10, digits = 0),
           popularity_score = round(((total_hours_elapsed / as.numeric(totalHours))
                                     / outlets) * 100 + 10, digits = 0))
  mapDf$total_sessions <- as.numeric(mapDf$total_sessions)
  mapDf$total_charged <- as.numeric(mapDf$total_charged)
  return(mapDf)
}

# Render functions ------------------------------------------------------------------------------------------------

mapId <- "regressionMap"

# Creates the default leaflet map without user input
handleDefaultRegressionMapCreation <- function(mapData) {
  radius <- 100
  leaflet() %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
    setView(lng = 4.32, lat = 52.05, zoom = 12) %>%
    defaultCircles(mapData, radius, color = "blue")
}

# Creates a leaflet map based on user input
handleRegressionMapCreation <- function(mapData) {
  pal <- colorBin("plasma", mapData$charged_kwh, 5, pretty = FALSE)
  radius <- 100
  color <- pal(mapData$charged_kwh)
  leafletProxy(mapId, data = mapData) %>% clearShapes() %>% defaultRegressionCircles(mapData, radius, color)
}

# This method handles the popup event
handleRegressionPopupCreation <- function(event, mapData) {
  leafletProxy(mapId) %>% clearPopups()
  if (is.null(event)) {
    return()
  }
  isolate({
    RegressionStationPopup(event$id, event$lat, event$lng, mapData)
  })
}

# Adds the default circle styling to a leaflet map
defaultRegressionCircles <- function(leaflet, mapData, radius, color) {
  leaflet %>% addCircles(
    lng = mapData$longitude,
    lat = mapData$latitude,
    radius = radius, stroke = FALSE,
    fillOpacity = 0.8, color = "#03f",
    layerId = which(mapData$longitude == mapData$longitude & mapData$latitude == mapData$latitude),
    fillColor = color)
}

geom_text(stat = "count", aes(label = as.character(round((..count..) / sum(..count..) * 100), digits = 2), "%"),
          position = position_stack(vjust = 0.5))

# Adds a popup to leaflet map when a node is clicked
RegressionStationPopup <- function(id, lat, lng, mapData) {
  selectedChargingPole <- mapData[id, ]
  content <- as.character(tagList(
    tags$h4("Location: ", selectedChargingPole$address),
    sprintf("Total charged kWh: %s")
  ))
  
  leafletProxy(mapId) %>% addPopups(lng, lat, content, layerId = id)
}
