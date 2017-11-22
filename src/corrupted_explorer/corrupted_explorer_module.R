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
              h4("Delete NA data"),
              uiOutput(ns("columnName")),
              h4("Corrupted data"),
              # checkboxGroupInput("checkGroup", 
              #                    h3("Checkbox group"), 
              #                    choices = list("corrupt" = TRUE, 
              #                                   "session" = TRUE),
              #                    selected = 1)),
              checkboxInput(ns("corrupt"), "delete", value = FALSE),
              h4("Have low session under 10"),
              checkboxInput(ns("session"), "delete", value = FALSE),
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
              withSpinner(plotOutput(ns("plot"))),
              title = "static data "
            )
          )
  )
}

corruptedExplorerModule <- function(input, output, session, data) {
  output$columnName <- renderUI({
    ns <- session$ns
    selectInput(ns("columnN"), textOutput("minimumReq"), as.list(names(data)), multiple = TRUE)
  })
  
  a <- reactive({
      showDataStatusPlot(data, input$columnN, input$corrupt, input$session)
  })
  
  output$plot <- renderPlot({
    if (input$action) {
      return(isolate(a()))
    }
    defaultPlot(data)
  })
}

defaultPlot <- function(data) {
  showDataStatusPlot(data)
}
