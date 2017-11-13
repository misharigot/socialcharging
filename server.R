library(shiny)
library(readr)
library(config)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)

config <- config::get(file = "config.yml")
source(config$baseClean)
source("map/map_renderer.R")

server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  df <- read_csv2(config$scDataset, col_names = FALSE)
  df <- cleanDataframe(df)

  # maybe a javascript to reset the ranges variable on active view change?
  # Single zoomable plot
  ranges <- reactiveValues(x = NULL, y = NULL)

  # Output ----------------------------------------------------------------------------------------------------------

  output$table1 <- renderDataTable({
    df
  })

  output$plot1 <- renderPlot({
    source("src/time_vs_kwh.R")
    return(plotTimeKwh() +
             coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE))
  })

  output$plot2 <- renderPlot({
    source("src/smart_charging_vs_kwh.R")
    return(plotMultiple())
  })

  output$plot3 <- renderPlot({
    source("src/kwh_vs_station.R")
    return(plotKwhPerStationPerDay())
  })

  output$plot4 <- renderPlot({
    source("src/timeframe_vs_sessions.R")
    return(multiplotTimeframes())
  })
  
  output$plot7 <- renderPlot({
    source("src/cars.R")
    if (input$plot7Input == "0") {
      plotPercentagePerCar()
    } else if (input$plot7Input == "1") {
      plotAverageChargedKwhPerCar()
    }
  })

  output$plot8 <- renderPlot({
    source("src/timeframe_vs_users.R")
    return(multiplotUserTimeframes())
  })

  output$map <- renderLeaflet({
    source("src/location_vs_kwh.R")
    handleDefaultMapCreation()
  })

  # Observers -------------------------------------------------------------------------------------------------------

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

  #Eventhandler for changing the data for the map
  observe({
    handleMapCreation(input$category)
  })

  #Eventhandler for Popups when clicking on circle
  observe({
    handlePopupCreation(input$map_shape_click)
  })
}
