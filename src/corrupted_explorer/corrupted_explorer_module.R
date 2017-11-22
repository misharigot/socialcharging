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
              h4("Omit NA data"),
              uiOutput(ns("valuesNA")),
              # checkboxGroupInput("checkGroup", 
              #                    h3("Checkbox group"), 
              #                    choices = list("corrupt" = TRUE, 
              #                                   "session" = TRUE),
              #                    selected = 1)),
              checkboxInput(ns("corruptDate"), "Corrupted dates", value = FALSE),
              checkboxInput(ns("session"), "Omit sessions with users having less than 10 total sessions", value = FALSE),
              actionButton(ns("action"), "Action"), width = 2
            ),
            # box(
            #   checkboxGroupInput("checkGroup",
            #                      h3("Checkbox group"),
            #                      choices = list("user_id" = "user_id",
            #                                     "session_id" = "session_id"))
            # ),
            
            # Data 
            box(
              withSpinner(plotOutput(ns("plot")))
            )
          )
  )
}

corruptedExplorerModule <- function(input, output, session, data) {
  output$valuesNA <- renderUI({
    ns <- session$ns
    selectInput(ns("valuesNA"), textOutput("minimumReq"), as.list(names(data)), multiple = TRUE)
  })
  
  a <- reactive({
      filters <- list(
        "valuesNA" = input$valuesNA,
        "corruptDate" = input$corruptDate,
        "session" = input$session
      )
      showDataStatusPlot(data, filters)
  })
  
  output$plot <- renderPlot({
    if (input$action) {
      return(isolate(a()))
    }
    defaultPlot(data)
  })
}

defaultPlot <- function(data) {
  filters <- list(
    "valuesNA" = NULL,
    "corruptDate" = FALSE,
    "session" = FALSE
  )
  showDataStatusPlot(data, filters)
}
