library(shiny)
library(readr)
library(config)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(plotly)

config <- config::get(file = "config.yml")
source(config$baseClean)
source("src/map/map_module.R")

server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  
  scData <- reactive({
    df <- read_csv2(config$scDataset, col_names = FALSE)
    df <- cleanDataframe(df)
    return(df)
  })
  
  callModule(module = mapModule, id = "map", data = scData())
  
  # maybe a javascript to reset the ranges variable on active view change?
  # Single zoomable plot
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Output ----------------------------------------------------------------------------------------------------------
  
  output$table1 <- renderDataTable({
    scData()
  })
  
  output$plot1 <- renderPlot({
    source("src/plots/time_vs_kwh.R")
    return(plotTimeKwh(scData()) +
             coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE))
  })
  
  output$plot2 <- renderPlot({
    source("src/plots/smart_charging_vs_kwh.R")
    return(plotMultiple(scData()))
  })
  
  output$plot3 <- renderPlot({
    source("src/plots/kwh_vs_station.R")
    return(plotKwhPerStationPerDay(scData()))
  })
  
  output$plot4 <- renderPlot({
    source("src/plots/timeframe_vs_sessions.R")
    return(multiplotTimeframes(scData()))
  })
  
  output$plot7 <- renderPlot({
    source("src/plots/cars.R")
    if (input$plot7Input == "0") {
      plotPercentagePerCar(scData())
    } else if (input$plot7Input == "1") {
      plotAverageChargedKwhPerCar(scData())
    }
  })
  
  output$plot8 <- renderPlot({
    source("src/plots/timeframe_vs_users.R")
    return(multiplotUserTimeframes(scData()))
  })
  
# Prediction plots ------------------------------------------------------------------------------------------------

  output$pred1 <- renderPlot({
    source("src/models/user_class.R")
    return(plotClassCountShiny(scData()))
  })
  
  output$pred2 <- renderPlotly({
    source("src/models/user_clust.R")
    return(plotUserCluster1(scData()))
  })

  output$plot3 <- renderPlotly({
    source("src/models/user_clust.R")
    return(plotUserCluster2(scData()))
  })
  
  output$pred4 <- renderPlot({
    source("src/models/regression_test.R")
    return(plotLinearModelsResult(scData()))
  })
  
  output$cor1 <- renderPlot({
    source("src/models/regression_test.R")
    return(plotCorrelationResult(scData()))
  })
  
  # Observers -------------------------------------------------------------------------------------------------------
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$dblclick, {
    brush <- input$brush
    if (!is.null(brush)) {
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
}
