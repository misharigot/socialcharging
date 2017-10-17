library(shiny)
library(readr)
library(config)

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)

config <- config::get(file = "config.yml")
source(config$baseClean)
source("src/location_vs_kwh.R")

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  df <- read_csv2(config$scDataset)
  df <- cleanDataframe(df)
  
  output$table1 <- renderDataTable({
    df
  })

  output$plot1 <- renderPlot({
    source("src/time_vs_kwh.R")
    plotTimeKwh()
  })

  output$plot2 <- renderPlot({
    source("src/smart_charging_vs_kwh.R")
    if (input$plot2Input == "0") {
      plotMultiple()
    } else if (input$plot2Input == "1") {
      plotKwhElapsedSmart()
    } else if (input$plot2Input == "2") {
      plotEffectiveChargingHoursElapsedSmart()
    } else if (input$plot2Input == "3") {
      plotKwhElapsed()
    } else if (input$plot2Input == "4") {
      plotEffectiveChargingHoursElapsed()
    }
  })

  output$plot3 <- renderPlot({
    source("src/kwh_vs_station.R")
    plotKwhPerStationPerDay()
  })

  output$plot4 <- renderPlot({
    source("src/timeframe_vs_sessions.R")
    return(multiplotTimeframes())
  })

  #map plot
  output$plot5 <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
      ) %>%
      setView(lng = 4.32, lat = 52.05, zoom = 12)
      
  })
  
  observe({
    categorySelected <- input$category
    mapData <- displayKwhData()
    
    if(categorySelected == "kwh"){
      mapData <- displayKwhData()
      radius <- mapData$total_charged / max(mapData$total_charged) * 300
      pal <- colorBin("plasma", mapData$total_charged, 7, pretty = TRUE)
    } 
    
    if(categorySelected == "Popularity"){
      mapData <- mostLeastPopular()
      radius <- mapData$total_sessions / max(mapData$total_sessions) * 300
      pal <- colorBin("plasma", mapData$total_charged, 7, pretty = TRUE)
    } 
    
    if(categorySelected == "Efficiency"){
      mapData <- mostLeastEfficient() 
      radius <- mapData$total_hours_elapsed / max(mapData$total_hours_elapsed) * 300
      pal <- colorBin("plasma", mapData$total_charged, 7, pretty = TRUE)
    }

    leafletProxy("plot5", data = mapData) %>%
      clearShapes() %>%
      addCircles(
        lng = mapData$longitude,
        lat = mapData$latitude,
        radius = radius, stroke=FALSE,
        fillOpacity = 0.8, color = "#03f",
        fillColor = pal(mapData$total_charged)) %>%
      addLegend("bottomleft", 
        pal=pal, 
        values=mapData$total_charged, 
        title="Total Charged kWh",
        layerId="colorLegend")
  })
}
