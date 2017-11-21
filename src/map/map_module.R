library(shiny)
library(ggplot2)
library(leaflet)
library(data.table)
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
                    h3("Filter controls"),
                    selectInput(ns("station_profiles"),
                                "Station profiles",
                                c(
                                  "All station profiles" = "all",
                                  "Profile based regression" = "profile_reg"
                                )
                    ),
                    selectInput(ns("user_profiles"),
                                "User profiles",
                                c(
                                  "All user profiles" = "all",
                                  "User based regression" = "user_reg"
                                )
                    ),
                    selectInput(ns("userId"),
                                "Users",
                                c("Show all" = "all")
                    ) 
        )
  )
  
}

# Server ----------------------------------------------------------------------------------------------------------
mapModule <- function(input, output, session, data) {
  # The default data without filters
  defaultMapData <- reactive({
    getMapData(plainData())
  })
  
  # Converts raw SC data into data prepped for the leaflet map
  mapData <- reactive({
    if (input$userId == "all") {
      getMapData(plainData())
    } else {
      plainData <- plainData()
      if (input$userId != "all") {
        plainData <- plainData %>% filter(user_id == input$userId)
      }
      if (input$station_profiles != "all") {
        plainData <- plainData %>% filter(station_profiles == input$station_profiles)
      }
      if (input$user_profiles != "all") {
        plainData <- plainData %>% filter(user_profile == input$user_profiles)
      }
      getMapData(plainData)
    }
  })
  
  # This reactive function should be called to use the data
  plainData <- reactive({
    data %>% filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed))
  })
  
  # The rendered leaflet map
  output$map <- renderLeaflet({
    handleDefaultMapCreation(input$size, input$color, mapData = defaultMapData())
  })
  
  # Update the user_id select input with the user_ids available
  observe({
    updateSelectInput(session, "userId", choices = c("Show all" = "all", plainData()$user_id))
  })
    
  # Updates the map when userId input changes
  observeEvent(input$userId, {
    handleMapCreation(input$size, input$color, mapData = mapData())
  })
  
  # Updates map when size input changes
  observeEvent(input$size, {
    handleMapCreation(input$size, input$color, mapData = mapData())
  })
  
  # Updates map when color input changes
  observeEvent(input$color, {
    handleMapCreation(input$size, input$color, mapData = mapData())
  })

  # Updates map with popup when a node is clicked
  observeEvent(input$map_shape_click, {
    handlePopupCreation(input$map_shape_click, mapData = mapData())
  })
}

# Functions -------------------------------------------------------------------------------------------------------

# source(config$baseClean)
# df <- read_csv2(config$scDataset, col_names = FALSE)
# 
# df <- cleanDataframe(df)

# Returns a data set prepared for the leaflet map, based on SC data
getMapData <- function(mapDf) {
  mapDf <- data.table(mapDf)
  coordDivision <- 100000000
  mapDf[, longitude := longitude / coordDivision]
  mapDf[, latitude := latitude / coordDivision]
  
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

# temp <- getMapData(df)
# temp <- temp %>%
#   filter(!is.na(total_charged))
# 
# temp2 <- as.data.frame(fivenum(temp$total_charged))
# temp2 <- temp2[-1]
# temp3 <- as.data.frame(fivenum(temp$total_charged))
# temp3 <- temp3[-nrow(temp3),]
# formatted <- as.list(paste(temp2,temp3, sep = " - "))


# Render functions ------------------------------------------------------------------------------------------------

mapId <- "map"

# Creates the default leaflet map without user input
handleDefaultMapCreation <-  function(sizeInput, colorInput, mapData) {
  if (length(sizeInput) == 0) {return()}
  if (length(colorInput) == 0) {return()}
  if (nrow(mapData) == 0) {return()}
  
  pal <- createPallete(mapData, colorInput)
  color <- createCircleColor(mapData, colorInput, pal)
  radius <- createCircleSize(mapData, sizeInput)
  values <- createLegendValues(mapData, colorInput)
  title <- createLegendTitle(colorInput)
  
  print(summary(values))
  
  # leftVal <- as.data.frame(fivenum(values))
  # leftVal <- leftVal[-1]
  # rightVal <- as.data.frame(fivenum(values))
  # rightVal <- rightVal[-nrow(rightVal),]
  # formatted <- as.list(paste(leftVal,rightVal, sep = " - "))
  
  leaflet() %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
    setView(lng = 4.32, lat = 52.05, zoom = 12) %>%
    defaultCircles(mapData, radius, pal(mapData$total_charged)) %>%
    addLegend("bottomright",
              pal = pal,
              values = fivenum(values),
              title = title,
              layerId = "colorLegend"
    )
}

# Creates a leaflet map based on user input
handleMapCreation <- function(sizeInput, colorInput, mapData) {
  if (length(sizeInput) == 0) {return()}
  if (length(colorInput) == 0) {return()}
  if (nrow(mapData) == 0) {return()}
  
  pal <- createPallete(mapData, colorInput)
  color <- createCircleColor(mapData, colorInput, pal)
  radius <- createCircleSize(mapData, sizeInput)
  values <- createLegendValues(mapData, colorInput)
  title <- createLegendTitle(colorInput)
  
  print(summary(values))
  
  # leftVal <- as.data.frame(fivenum(values))
  # leftVal <- leftVal[-1]
  # rightVal <- as.data.frame(fivenum(values))
  # rightVal <- rightVal[-nrow(rightVal),]
  # formatted <- as.list(paste(leftVal,rightVal, sep = " - "))
  
  leafletProxy(mapId, data = mapData) %>% 
    clearShapes() %>% 
    defaultCircles(mapData, radius, color) %>%
    addLegend("bottomright",
              pal = pal,
              values = fivenum(values),
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

# geom_text(stat = "count", aes(label = as.character(round((..count..) / sum(..count..) * 100), digits = 2), "%"),
#           position = position_stack(vjust = 0.5))

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