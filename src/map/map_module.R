library(shiny)
library(ggplot2)
library(leaflet)
library(shinyjs)
source("src/helpers/coordinate_helper.R")
source("src/map/map_functions.R")

# UI --------------------------------------------------------------------------------------------------------------
mapModuleUI <- function(id) {
  ns <- NS(id)
  div(class = "outer",
      tags$head(
        # Include our custom CSS
        includeCSS("src/map/styles.css"),
        includeScript("src/map/gomap.js")
      ),
      useShinyjs(),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput(ns("map"), width = "100%", height = "100%"),
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
                    h3("Size"),
                    selectInput(ns("size"),
                                 "", 
                                 choices = c(
                                   "Charged kWh" = "charged_kwh",
                                   "Elapsed hours" = "total_hours_elapsed",
                                   "Amount of sessions" = "total_sessions",
                                   "Occupation percentage" = "occ_perc",
                                   "Efficiency percentage" =  "eff_perc",
                                   "Users per station" = "users_station"
                                 ),
                                selected = "occ_perc"
                    ),
                    tags$hr(),
                    h3("Color"),
                    selectInput(ns("color"),
                                 "", 
                                 choices = c(
                                   "Charged kWh" = "charged_kwh",
                                   "Elapsed hours" = "total_hours_elapsed",
                                   "Amount of sessions" = "total_sessions",
                                   "Occupation percentage" = "occ_perc",
                                   "Efficiency percentage" =  "eff_perc",
                                   "Users per station" = "users_station"
                                 ),
                                selected = "charged_kwh"
                    ),
                    tags$hr(),
                    filterUI("filterSelection")
      ),
      absolutePanel(
        actionButton("btnHide", "show/hide"),
        id = "session-table",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = FALSE,
        top = "auto",
        left = 250,
        right = "auto",
        bottom = -15,
        width = 1300,
        height = "auto",
        style = "info",
        h3("Station sessions"),
        div(style = "height: 200px; overflow-y: auto;", tableOutput(ns("stationTable")))
      )
  )
}


filterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Filter controls"),
    selectInput(ns("station_profiles"),
                "Station profiles",
                c(
                  "All station profiles" = "station_profiles",
                  "Profile based regression" = "profile_reg"
                )
    ),
    selectInput(ns("user_profiles"),
                "User profiles",
                c(
                  "All user profiles" = "user_profiles",
                  "User based regression" = "user_reg"
                )
    ),
    uiOutput("user_selection")
  )
}

# Server ----------------------------------------------------------------------------------------------------------
mapModule <- function(input, output, session, data) {
  # Converts raw SC data into data prepped for the leaflet map
  mapData <- reactive({
    getMapData(data)
  })
  
  temp <- reactive({
    prepTableData(data)
  })
  
  # The rendered leaflet map
  output$map <- renderLeaflet({
    handleDefaultMapCreation(mapData = mapData())
  })
  
  # Updates map when size input changes
  observeEvent(input$size, {
    handleMapCreation(input$size, input$color, mapData = mapData())
  })
  
  # Updates map when color input changes
  observeEvent(input$color, {
    handleMapCreation(input$size, input$color, mapData = mapData())
  })
  
  # Updates map with popup and updated table when a node is clicked
  observeEvent(input$map_shape_click, {
    handlePopupCreation(input$map_shape_click, mapData = mapData())
  })
  
  tableData <- reactive({
    if (is.null(input$map_shape_click)) {return(NULL)}
    print(input$map_shape_click)
    sessions <- temp()
    isolate({
      sessions <- sessions %>%
        filter(latitude == input$map_shape_click$lat,
               longitude == input$map_shape_click$lng)
      return(sessions)
    })
  })

  # WIP table output
  output$stationTable <- renderTable({
    tableData()
  })
  
  observeEvent(input$btnHide, {
    shinyjs::toggle("session-table")
  }) 
  
}

# Functions -------------------------------------------------------------------------------------------------------
# Returns a data set prepared for the leaflet map, based on SC data
prepTableData <- function(dataf) {
  dataf <- dataf %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed)) %>%
    select(latitude, longitude, session_id, user_id, start_date, end_date, charged_kwh, hours_elapsed)
  
  dataf$latitude <- sapply(dataf$latitude, formatCoordinate, "latitude")
  dataf$longitude <- sapply(dataf$longitude, formatCoordinate, "longitude")
  dataf$latitude <- as.numeric(dataf$latitude)
  dataf$longitude <- as.numeric(dataf$longitude)
  return(dataf)
}

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

mapId <- "map"

# Creates the default leaflet map without user input
handleDefaultMapCreation <- function(mapData) {
  pal <- createPallete(mapData)
  color <- createCircleColor(mapData, pal = pal)
  radius <- createCircleSize(mapData)
  values <- createLegendValues(mapData)
  title <- createLegendTitle()
  
  leaflet() %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
    setView(lng = 4.32, lat = 52.05, zoom = 12) %>%
    defaultCircles(mapData, radius, pal(mapData$total_charged)) %>%
    addLegend("bottomright",
              pal = pal,
              values = values,
              title = title,
              layerId = "colorLegend"
    )
}

# Creates a leaflet map based on user input
handleMapCreation <- function(sizeInput, colorInput, mapData) {
  if (length(sizeInput) == 0) {return()}
  if (length(colorInput) == 0) {return()}
  
  pal <- createPallete(mapData, colorInput)
  color <- createCircleColor(mapData, colorInput, pal)
  radius <- createCircleSize(mapData, sizeInput)
  values <- createLegendValues(mapData, colorInput)
  title <- createLegendTitle(colorInput)

  leafletProxy(mapId, data = mapData) %>% 
    clearShapes() %>% 
    defaultCircles(mapData, radius, color) %>%
    addLegend("bottomright",
              pal = pal,
              values = values,
              title = title,
              layerId = "colorLegend")
}

# This method handles the popup event
handlePopupCreation <- function(event, mapData) {
  leafletProxy(mapId) %>% clearPopups()
  if (is.null(event)) {
    return()
  }
  isolate({
    chargingStationPopup(event$id, event$lat, event$lng, mapData)
  })
}

# Adds the default circle styling to a leaflet map
defaultCircles <- function(leaflet, mapData, radius, color) {
  leaflet %>% addCircles(
    lng = mapData$longitude,
    lat = mapData$latitude,
    radius = radius,
    stroke = FALSE,
    fillOpacity = 0.8,
    color = "#03f",
    layerId = which(mapData$longitude == mapData$longitude & mapData$latitude == mapData$latitude),
    fillColor = color)
}

geom_text(stat = "count", aes(label = as.character(round((..count..) / sum(..count..) * 100), digits = 2), "%"),
          position = position_stack(vjust = 0.5))

# Adds a popup to leaflet map when a node is clicked
chargingStationPopup <- function(id, lat, lng, mapData) {
  selectedChargingPole <- mapData[id, ]
  content <- as.character(tagList(
    tags$h4("Location: ", selectedChargingPole$address),
    sprintf("Total charged kWh: %s", round(selectedChargingPole$total_charged, digits = 2)), tags$br(),
    sprintf("Total elapsed hours: %s", round(selectedChargingPole$total_hours_elapsed, digits = 2)), tags$br(),
    sprintf("Total effective hours: %s", round(selectedChargingPole$total_effective_charging, digits = 2)), tags$br(),
    sprintf("Station outlets: %s", selectedChargingPole$outlets), tags$br(),
    sprintf("Total sessions: %s", selectedChargingPole$total_sessions), tags$br(),
    sprintf("Total users: %s", selectedChargingPole$total_users)
  ))
  
  leafletProxy(mapId) %>% addPopups(lng, lat, content, layerId = id)
}



