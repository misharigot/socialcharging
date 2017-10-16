library(shiny)
library(readr)
library(shinydashboard)
library(config)
config <- config::get(file = "config.yml")
source(config$baseClean)

ui <- dashboardPage(
  dashboardHeader(title = "Social Charging"),
  dashboardSidebar(
    menuItem("Raw data", tabName = "raw", icon = icon("th")),
    menuItem("Charts", tabName = "raw", icon = icon("bar-chart"),
             menuSubItem("Time elapsed vs kWh charged", tabName = "chart1"),
             menuSubItem("Smart vs non-smart charging", tabName = "chart2"),
             menuSubItem("kWh vs charging stations", tabName = "chart3"),
             menuSubItem("Timeframe vs charging sessions", tabName = "chart4")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "raw",
        fluidRow(
          box(
            title = "Social Charging dataset", status = "success", solidHeader = TRUE, width = 12,
            div(style = 'overflow-x: scroll', dataTableOutput("table1"))
          )
        )
      ),
      tabItem(tabName = "chart1",
              fluidRow(
                box(plotOutput("plot1"), width = 12)
              )
      ),
      tabItem(tabName = "chart2",
              fluidRow(
                box(
                  title = "Controls", width = 5, solidHeader = TRUE, status = "primary",
                  selectInput(inputId = "plot2Input", 
                              label = "Select a chart",
                              choices = c("Multiple plots" = "0",
                                          "kWh elapsed - smart" = "1",
                                          "Effective charging hours - smart" = "2",
                                          "kWh elapsed - non-smart" = "3",
                                          "Effective charging hours - non-smart" = "4"
                              ))
                )
              ),
              fluidRow(box(plotOutput("plot2"), title = "Smart charging ", width = 12))
      ),
      tabItem(tabName = "chart3",
              fluidRow(
                box(plotOutput("plot3"), width = 12)
              )
      ),
      tabItem(tabName = "chart4",
              fluidRow(
                fluidRow(
                  box(plotOutput("plot4"), width = 12)
                )
              )
      )
    )
  )
)

server <- function(input, output) {
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
    if(input$plot2Input == "0") {
      plotMultiple()
    } else if (input$plot2Input == "1") {
      plotKwhElapsedSmart()
    } else if (input$plot2Input == "2") {
      plotEffectiveChargingHoursElapsedSmart()
    } else if(input$plot2Input == "3") {
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
}

shinyApp(ui, server)
