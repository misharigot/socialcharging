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

server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  df <- read_csv2(config$scDataset)
  df <- cleanDataframe(df)
  
  output$table1 <- renderDataTable({
    df
  })

  # maybe a javascript to reset the ranges variable on active view change?
  # Single zoomable plot
  ranges <- reactiveValues(x = NULL, y = NULL)

  output$plot1 <- renderPlot({
    source("src/time_vs_kwh.R")
    return(plotTimeKwh() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE))
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$dblclick, {
    brush <- input$brush
    if (!is.null(brush)) {
      print(input)
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  observeEvent(input$reset_input, {
    ranges$x <- NULL
    ranges$y <- NULL
  })

  observeEvent(input$reset_input_1, {
    ranges$x <- NULL
    ranges$y <- NULL
  })

  output$plot2 <- renderPlot({
    source("src/smart_charging_vs_kwh.R")
    if (input$plot2Input == "0") {
      return(plotMultiple())
    } else if (input$plot2Input == "1") {
      return(plotKwhElapsedSmart() +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE))
    } else if (input$plot2Input == "2") {
      return(plotEffectiveChargingHoursElapsedSmart() +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE))
    } else if (input$plot2Input == "3") {
      return(plotKwhElapsed() +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE))
    } else if (input$plot2Input == "4") {
      return(plotEffectiveChargingHoursElapsed() +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE))
    }
  })

  output$plot3 <- renderPlot({
    source("src/kwh_vs_station.R")
    return(plotKwhPerStationPerDay())
  })

  output$plot4 <- renderPlot({
    source("src/timeframe_vs_sessions.R")
    return(multiplotTimeframes())
  })

  source("map/map_renderer.R")
  
  #Load Default values in map
  output$plot5 <- renderLeaflet({
    handleDefaultMapCreation()
  })
  
  #Eventhandler for changing the data for the map
  observe({
    handleMapCreation(input$category)
  })
  
  #Eventhandler for Popups when clicking on circle
  observe({
    handlePopupCreation(input$plot5_shape_click)
  })

  output$plot6 <- renderPlot({
    source("src/stations_per_user.R")
    return(plotUsersPerDifferentStations())
  })
}
