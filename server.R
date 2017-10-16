library(shiny)
library(readr)
library(config)
config <- config::get(file = "config.yml")
source(config$baseClean)

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
}
