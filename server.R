library(shiny)
library(readr)
library(config)

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)

config <- config::get(file = "config.yml")
source(config$baseClean)

server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  df <- read_csv2(config$scDataset)
  df <- cleanDataframe(df)

  source("src/location_vs_kwh.R")
  colorData <- CreateDataForMapPlot()

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

    radius <- colorData$total / max(colorData$total) * 300
    pal <- colorBin("plasma", colorData$total, 7, pretty = FALSE)

    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 4.32, lat = 52.05, zoom = 12) %>%
      addCircles(
        lng = colorData$longitude,
        lat = colorData$latitude,
        radius = radius, stroke=FALSE,
        fillOpacity=0.8, color = "#03f",
        fillColor=pal(colorData$total)) %>%
      addLegend("bottomleft", pal=pal, values=colorData$total, title="Total Charged kWh",
                layerId="colorLegend")
  })
  
  output$plot6 <- renderPlot({
    source("src/stations_per_user.R")
    return(plotUsersPerDifferentStations())
  })
  
  output$plot7 <- renderPlot({
    source("src/carPlot.R")
    if (input$plot7Input == "0") {
      plotPersantagePerCar()
    } else if (input$plot7Input == "1") {
      plotTotaltotalChargingPerCar()
    } 
    
  })
}
