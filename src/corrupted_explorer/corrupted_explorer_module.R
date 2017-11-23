# 91 visulization statistic data
library(config)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(plotly)
library(shiny)
source("src/plots/visulization_statistic_data.R")

corruptedExplorerModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "static",
          fluidRow(
            # Filters
            box(
              h2("Filters"),
              withSpinner(uiOutput(ns("valuesNA")), proxy.height = "50px"),
              checkboxInput(ns("corruptDate"), "Remove corrupted dates", value = FALSE),
              checkboxInput(ns("zeroCharged"), "Remove sessions with 0 charged kWh", value = FALSE),
              checkboxInput(ns("session"), "Remove sessions with users having less than 10 total sessions", value = FALSE),
              checkboxInput(ns("normalHoursElapsed"), "Remove sessions with more than 100 hours elapsed", value = FALSE),
              actionButton(ns("action"), "Action"),
              width = 5
            ),
            # Data 
            box(
              withSpinner(plotOutput(ns("plot"))),
              h3(textOutput(ns("ratio")))
            )
          )
  )
}

corruptedExplorerModule <- function(input, output, session, data) {
  output$valuesNA <- renderUI({
    ns <- session$ns
    selectInput(ns("valuesNA"), label = h5("Select columns to omit NA data for"), as.list(names(data)), multiple = TRUE)
  })
  
  output$ratio <- renderText({
    if (input$action) {
      return(isolate(ratio()))
    }
    defaultRatio(data)
  })
  ratio <- reactive({
    filters <- list(
      "valuesNA" = input$valuesNA,
      "corruptDate" = input$corruptDate,
      "zeroCharged" = input$zeroCharged,
      "session" = input$session,
      "normalHoursElapsed" = input$normalHoursElapsed
    )
    totalCount <- nrow(data)
    filteredCount <- getFilteredDataCount(data, filters)
    ratio <- paste(filteredCount, totalCount, sep = "/")
    paste0(ratio, " sessions left.")
  })
  
  filteredData <- reactive({
      filters <- list(
        "valuesNA" = input$valuesNA,
        "corruptDate" = input$corruptDate,
        "zeroCharged" = input$zeroCharged,
        "session" = input$session,
        "normalHoursElapsed" = input$normalHoursElapsed
      )
      showDataStatusPlot(data, filters)
  })
  
  output$plot <- renderPlot({
    if (input$action) {
      return(isolate(filteredData()))
    }
    defaultPlot(data)
  })
}

defaultPlot <- function(data) {
  filters <- list(
    "valuesNA" = NULL,
    "corruptDate" = FALSE,
    "zeroCharged" = FALSE,
    "session" = FALSE,
    "normalHoursElapsed" = FALSE
  )
  showDataStatusPlot(data, filters)
}

defaultRatio <- function(data) {
 
  totalCount <- nrow(data)
  ratio <- paste(totalCount, totalCount, sep = "/")
  paste0(ratio, " sessions left.")
}
