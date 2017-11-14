library(shiny)
library(ggplot2)

# UI --------------------------------------------------------------------------------------------------------------

myMapDataUI <- function(id) {
  ns <- NS(id)
  div(class = "outer",
      tags$head(
        # Include our custom CSS
        includeCSS("src/map/styles.css"),
        includeScript("src/map/gomap.js")
      ),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput(ns("map"), width = "100%", height = "100%"),
      absolutePanel(id = "controls",
                    class = "panel panel-default",
                    fixed = TRUE,
                    draggable = TRUE,
                    top = 60,
                    left = "auto",
                    right = 20,
                    bottom = "auto",
                    width = 330,
                    height = "auto",
                    h2("Filter controls"),
                    selectInput("category",
                                "Category",
                                c("Charged kWh per station", "Occupation percentage",
                                  "Efficiency percentage", "Users per station")
                    ),
                    h5("The category determines the size of the circles")
      )
  )
  
}

# Server ----------------------------------------------------------------------------------------------------------

source("src/map/map_renderer.R")

myMapData <- function(input, output, session, data) {
  mapData <- reactive({
    getMapData(data)
  })
  
  #Eventhandler for changing the data for the map
  # observe({
  #   handleMapCreation(input$category, mapData = mapData())
  # })
  # 
  # #Eventhandler for Popups when clicking on circle
  # observe({
  #   handlePopupCreation(input$map_shape_click, mapData = mapData())
  # })
  
  return(mapData)
}

# Functions -------------------------------------------------------------------------------------------------------

getMapData <- function(scData) {
  print("cleaning map data")
  mapDf <- scData %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed))
  
  mapDf$latitude <- sapply(mapDf$latitude, function(x){
    insert <- "."[order(3)]
    index <- sort(3)
    paste(interleave(split_str_by_index(x, 3), "."), collapse = "")
  })
  
  mapDf$longitude <- sapply(mapDf$longitude, function(x){
    insert <- "."[order(2)]
    index <- sort(2)
    paste(interleave(split_str_by_index(x, 2), "."), collapse = "")
  })
  
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
}

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


# render functions ------------------------------------------------------------------------------------------------


# Id for ui.R
mapId <- "map"

# This method handles the default content of the map like circles and legends.
handleDefaultMapCreation <- function(userInput, mapData) {
  radius <- mapData$total_charged / max(mapData$total_charged) * 300
  pal <- colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
  
  leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
    ) %>%
    setView(lng = 4.32, lat = 52.05, zoom = 12) %>%
    addCircles(
      lng = mapData$longitude,
      lat = mapData$latitude,
      radius = radius, stroke = FALSE,
      fillOpacity = 0.8, color = "#03f",
      layerId = which(mapData$longitude == mapData$longitude & mapData$latitude == mapData$latitude),
      fillColor = pal(mapData$total_charged)) %>%
    addLegend("bottomright",
              pal = pal,
              values = mapData$total_charged,
              title = "Total Charged kWh",
              layerId = "colorLegend"
    )
}

# This method handles the content of the map like circles and legends.
handleMapCreation <- function(userInput, mapData) {
  categorySelected <- userInput
  if (!(categorySelected == "Users per station")) {
    
    if (categorySelected == "Charged kWh per station") {
      radius <- mapData$total_charged / max(mapData$total_charged) * 300
      pal <-  colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
    }
    
    if (categorySelected == "Occupation percentage") {
      radius <- mapData$popularity_score / max(mapData$popularity_score) * 300
      pal <- colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
    }
    
    if (categorySelected == "Efficiency percentage") {
      radius <- mapData$efficiency_score / max(mapData$efficiency_score) * 300
      pal <- colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
    }
    
    leafletProxy(mapId, data = mapData) %>%
      clearShapes() %>%
      addCircles(
        lng = mapData$longitude,
        lat = mapData$latitude,
        radius = radius, stroke = FALSE,
        fillOpacity = 0.8, color = "#03f",
        layerId = which(mapData$longitude == mapData$longitude & mapData$latitude == mapData$latitude),
        fillColor = pal(mapData$total_charged)) %>%
      addLegend("bottomright",
                pal = pal,
                values = mapData$total_charged,
                title = "Total Charged kWh",
                layerId = "colorLegend"
      )
  } else {
    radius <- mapData$total_users / max(mapData$total_users) * 300
    
    leafletProxy(mapId, data = mapData) %>%
      clearShapes() %>%
      addCircles(
        lng = mapData$longitude,
        lat = mapData$latitude,
        radius = radius, stroke = FALSE,
        fillOpacity = 0.7, color = "#03f",
        layerId = which(mapData$longitude == mapData$longitude & mapData$latitude == mapData$latitude),
        fillColor = "red")
  }
}

geom_text(stat = "count", aes(label = as.character(round((..count..) / sum(..count..) * 100), digits = 2), "%"),
          position = position_stack(vjust = 0.5))

# This method handles the content of the popup when circles are clicked on the map.
chargingStationPopup <- function(id, lat, lng, mapData) {
  selectedChargingPole <- mapData[id, ]
  content <- as.character(tagList(
    tags$h4("Location: ", selectedChargingPole$address),
    sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br(),
    sprintf("Total elapsed hours: %s", selectedChargingPole$total_hours_elapsed), tags$br(),
    sprintf("Total effective hours: %s", selectedChargingPole$total_effective_charging), tags$br(),
    sprintf("Station outlets: %s", selectedChargingPole$outlets), tags$br(),
    sprintf("Total sessions: %s", selectedChargingPole$total_sessions), tags$br(),
    sprintf("Total users: %s", selectedChargingPole$total_users)
  ))
  
  leafletProxy(mapId) %>%
    addPopups(lng, lat, content, layerId = id)
}

# This method handles the popup event
handlePopupCreation <- function(event, mapData) {
  leafletProxy(mapId) %>% clearPopups()
  if (is.null(event))
    return()
  isolate({
    chargingStationPopup(event$id, event$lat, event$lng, mapData)
  })
}
