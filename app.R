library(shiny)
library(readr)
library(shinydashboard)
library(config)
config <- config::get(file = "config.yml")
source(config$baseClean)

ui <- dashboardPage(
  dashboardHeader(title = "Social Charging"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Raw data", tabName = "raw", icon = icon("th")),
    menuItem("Charts", tabName = "raw", icon = icon("bar-chart"),
             menuSubItem("Time vs KWH", tabName = "chart1"),
             menuSubItem("lorem", tabName = "chart2"),
             menuSubItem("lorem", tabName = "chart3"),
             menuSubItem("lorem", tabName = "chart4"),
             menuSubItem("lorem", tabName = "chart5")
             )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow()
      ),
      tabItem(tabName = "raw",
              h2("Raw Data"),
              tableOutput("table1")
      ),
      tabItem(tabName = "chart1",
              fluidRow(
                box(plotOutput("plot1"))
              )
      ),
      tabItem(tabName = "chart2",
              fluidRow(
                box(plotOutput("plot2"))
              )
      ),
      tabItem(tabName = "chart3",
              fluidRow(
                box(plotOutput("plot3"))
              )
      ),
      tabItem(tabName = "chart4",
              fluidRow(
                box(plotOutput("plot4", height = 250))
              )
      ),
      # Seventh tab content
      tabItem(tabName = "chart5",
              fluidRow(
                box(plotOutput("plot5", height = 250))
              )
      )
    )
  )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)

  df <- read_csv2(config$scDataset)
  df <- cleanDataframe(df)
  
  output$table1 <- renderTable({
    df
  })
  
  output$plot1 <- renderPlot({
    source("src/time_vs_kwh.R")
    plotTimeKwh()
  })
  
  output$plot2 <- renderPlot({
    set.seed(122)
    histdata <- rnorm(500)
    
    data <- histdata
    hist(data)
  })
  
  output$plot3 <- renderPlot({
    set.seed(122)
    histdata <- rnorm(500)
    
    data <- histdata
    hist(data)
  })
  
  output$plot4 <- renderPlot({
    set.seed(122)
    histdata <- rnorm(500)
    
    data <- histdata
    hist(data)
  })
  
  output$plot5 <- renderPlot({
    set.seed(122)
    histdata <- rnorm(500)
    
    data <- histdata
    hist(data)
  })
}

shinyApp(ui, server)
