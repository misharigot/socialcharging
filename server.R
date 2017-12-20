library(shiny)
library(readr)
library(config)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(plotly)
library(corrplot)
library(timevis)

config <- config::get(file = "config.yml")
source(config$baseClean)
source("src/map/map_module.R")
source("src/corrupted_explorer/corrupted_explorer_module.R")
source("src/models/regression_test.R")

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  # session$onSessionEnded(stopApp)

  scData <- reactive({
    df <- read_csv2(config$scDataset, col_names = FALSE)
    df <- cleanDataframe(df)
    return(df)
  })

  regressionData <- reactive({
    df <- read.csv2(config$dataFolder, sep = ",")
    head(df)
    df <- changeStructures(df)
    head(df)
    return(df)
  })
  
  predictedValuesDf <- reactive({
    source("src/models/extended_classification.R")
    getPredictedValuesDf(scData())
  })

  # Returns the numberfied dataframe
  numberfiedDf <- reactive({
    source("src/models/Interactive_correlation.R")
    corDf <- convertDfToNumeric(sessionClassificationDf(cleanDf(scData())))
    return(corDf)
  })

  # Returns the name of the numberfied dataframe
  dfNames <- reactive({
    return(names(numberfiedDf()))
  })

  callModule(module = mapModule, id = "map", data = regressionData())
  callModule(module = corruptedExplorerModule, id = "corrupt", data = scData())

  output$user_selection <- renderUI({
    selectInput("users",
                "Select a user",
                isolate(as.vector(scData()$user_id))
    )
  })

  # Single zoomable plot
  ranges <- reactiveValues(x = NULL, y = NULL)

  # Output ----------------------------------------------------------------------------------------------------------

  output$corColumns <- renderUI({
    selectInput("columns", textOutput("minimumReq"), as.list(dfNames()), multiple = TRUE)
  })

  output$table1 <- renderDataTable({
    scData()
  })

  output$minimumReq <- renderText({
    if (length(input$columns) < 2) {
      "Select at least 2 columns"
    } else {
      "Select columns"
    }
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

  output$pred3 <- renderPlotly({
    source("src/models/user_clust.R")
    return(plotUserCluster2(scData()))
  })

  output$cor1 <- renderPlot({
    if (length(input$columns) < 2) {
      source("src/models/Interactive_correlation.R")
      return(plotCorrelationplot(scData()))
    } else {
      return(corrplot.mixed(cor(numberfiedDf()[,input$columns])))
    }
  })

  output$pred6 <- renderPlotly({
    source("src/models/cluster_charging_station.R")
    return(createStationClusterPlot(scData()))
  })

  output$pred7 <- renderPlot({
    source("src/models/station_classification.R")
    return(showDistributionPlot(scData()))
  })
  
# Weekschedule ----------------------------------------------------------------------------------------------------
  
  output$weekschedule <- renderTimevis({
    source("src/helpers/date_helper.R")
    source("src/helpers/data_helper.R")
    source("src/models/charging_template.R")
    
    nextMonday <- nextWeekday(1)
    nextSunday <- nextWeekday(7)
    
    predictedValuesDf <- predictedValuesDf()
    data <- data.frame()
    
    if (!input$wsProfileSelect == "Select a user profile") {
      selectedValues <- predictedValuesDf %>% filter(formatted_class == input$wsProfileSelect)
      timelineData <- convertSessionsToTimelineData(selectedValues)
  
      data <- data.frame(
        content = templateCharging(timelineData$rounded_efficiency, timelineData$formatted_kwh, 
                                   stripDate(timelineData$start_datetime, "%Y-%m-%d %H:%M:%S"), 
                                   stripDate(timelineData$end_datetime, "%Y-%m-%d %H:%M:%S")),
        start   = timelineData$start_datetime, # 2017-12-26 10:00:00
        end     = timelineData$end_datetime, # 2017-12-26 13:32:00
        title = c(paste0("Start time: ", timelineData$start_datetime,
  " \nEnd time: ", timelineData$end_datetime,
  " \nCharged kWh: ", timelineData$formatted_kwh,
  " \nPredict: ", timelineData$pred_hours_elapsed))
      )
    }
    
    config <- list(
      editable = FALSE,
      orientation = "top",
      snap = NULL,
      height = 200,
      margin = list(item = 10, axis = 50),
      showCurrentTime = FALSE,
      zoomable = TRUE,
      moveable = TRUE,
      min = nextMonday,
      max = nextSunday + 1
    )
    
    timevis(data, showZoom = TRUE, fit = TRUE, options = config) %>%
      setWindow(nextMonday, nextSunday + 1, options = list(animation = FALSE))
  })
  
  output$table2 <- renderTable({
    source("src/week_schedule/week_schedule.R")
    if (input$action) {
      isolate(selectData(regressionData(), input$text, input$dates))
    }
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
  
  # Update the select input with unique user classifications/profiles to select from.
  observe({
    updateSelectInput(session,
                      "wsProfileSelect",
                      choices = isolate(distinct(predictedValuesDf())$formatted_class))
  })
}
