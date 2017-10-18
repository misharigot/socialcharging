# This scripts handles all information for the map plot
source("src/location_vs_kwh.R")

# This method handles the content of the map like circles and legends.
handleMapCreation <- function(userInput) {
  categorySelected <- userInput
  mapData <- cleanMapData()

  if(categorySelected == "kwh"){
    radius <- mapData$total_charged / max(mapData$total_charged) * 300
    pal <- colorBin("plasma", mapData$total_charged, 7, pretty = TRUE)
  }

  if(categorySelected == "Popularity"){
    radius <- mapData$popularity_score / max(mapData$popularity_score) * 300
    pal <- colorBin("plasma", mapData$total_charged, 7, pretty = TRUE)
  }

  if(categorySelected == "Efficiency"){
    radius <- mapData$efficiency_score / max(mapData$efficiency_score) * 300
    pal <- colorBin("plasma", mapData$total_charged, 7, pretty = TRUE)
  }

  leafletProxy("plot5", data = mapData) %>%
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
              layerId = "colorLegend")
}

# This method handles the content of the popup when circles are clicked on the map.
chargingStationPopup <- function(id, lat, lng, cat) {
  cleanedMapData <- cleanMapData()

  if (cat == "kwh") {
    selectedChargingPole <- cleanedMapData[id, ]
    content <- as.character(tagList(
      tags$h4("Location: ", selectedChargingPole$address),
      sprintf("Station outlets: %s", selectedChargingPole$outlets), tags$br(),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br(),
      sprintf("Total sessions: %s", selectedChargingPole$total_sessions), tags$br(),
      sprintf("Total elapsed hours: %s", selectedChargingPole$total_hours_elapsed), tags$br(),
      sprintf("Efficiency score: %s", selectedChargingPole$efficiency_score), tags$br(),
      sprintf("Popularity score: %s", selectedChargingPole$popularity_score)
    ))
    leafletProxy("plot5") %>%
      addPopups(lng, lat, content, layerId = id)
  }

  if (cat == "Popularity") {
    selectedChargingPole <- cleanedMapData[id, ]
    content <- as.character(tagList(
      tags$h4("Location: ", selectedChargingPole$address),
      sprintf("Station outlets: %s", selectedChargingPole$outlets), tags$br(),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br(),
      sprintf("Total sessions: %s", selectedChargingPole$total_sessions), tags$br(),
      sprintf("Total elapsed hours: %s", selectedChargingPole$total_hours_elapsed), tags$br(),
      sprintf("Efficiency score: %s", selectedChargingPole$efficiency_score), tags$br(),
      sprintf("Popularity score: %s", selectedChargingPole$popularity_score)
    ))
    leafletProxy("plot5") %>%
      addPopups(lng, lat, content, layerId = id)
  }

  if (cat == "Efficiency") {
    selectedChargingPole <- cleanedMapData[id, ]
    content <- as.character(tagList(
      tags$h4("Location: ", selectedChargingPole$address),
      sprintf("Station outlets: %s", selectedChargingPole$outlets), tags$br(),
      sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br(),
      sprintf("Total sessions: %s", selectedChargingPole$total_sessions), tags$br(),
      sprintf("Total elapsed hours: %s", selectedChargingPole$total_hours_elapsed), tags$br(),
      sprintf("Efficiency score: %s", selectedChargingPole$efficiency_score), tags$br(),
      sprintf("Popularity score: %s", selectedChargingPole$popularity_score)
    ))
    leafletProxy("plot5") %>%
      addPopups(lng, lat, content, layerId = id)
  }
}

# This method handles the popup event
handlePopupCreation <- function(event, category) {
  leafletProxy("plot5") %>% clearPopups()
  print(event)
  if (is.null(event))
    return()
  isolate({
    chargingStationPopup(event$id, event$lat, event$lng, category)
  })
}