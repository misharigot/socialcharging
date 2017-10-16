library(shiny)
library(readr)
library(shinydashboard)
library(config)

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)

vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

config <- config::get(file = "../config.yml")
source(config$baseClean)

ui <- dashboardPage(
  dashboardHeader(title = "Social Charging"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Raw data", tabName = "raw", icon = icon("th")),
    menuItem("Charts", tabName = "raw", icon = icon("bar-chart"),
             menuSubItem("Time vs KWH", tabName = "chart1"),
             menuSubItem("Smart vs not Smart", tabName = "chart2"),
             menuSubItem("Interactive Map", tabName = "chart3"),
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
              fluidRow(
                box(
                  title = "Social Charging dataset", status = "success", solidHeader = TRUE, width = 12,
                  div(style = 'overflow-x: scroll', DT::dataTableOutput("table1"))
                )
              )
      ),
      tabItem(tabName = "chart1",
              fluidRow(
                box(plotOutput("plot1"))
              )
      ),
      tabItem(tabName = "chart2",
              fluidRow(
                box(
                  plotOutput("plot2")
                    ),
                box(
                  title = "Controls", width = 4, solidHeader = TRUE, status = "primary",
                  selectInput("SmartVsNotSmart", "Smart/Not Smart:",
                              choices = c("Smart/not Smart" = "smart",
                                "Session/kWh Smart" = "sessionKwh",
                                "Effective/Elapsed Smart" = "effectiveElapsed",
                                "Session/kWh not Smart" = "sessionKwhNS",
                                "Effective/Elapsed not Smart" = "effectiveElapsedNS"))
                    )
              )
      ),
      tabItem(tabName = "chart3",
              div(class="outer",
                  tags$head(
                    # Include our custom CSS
                    includeCSS("styles.css"),
                    includeScript("gomap.js")
                  ),
                  # If not using custom CSS, set height of leafletOutput to a number instead of percent
                  leafletOutput("map", width="100%", height="100%")
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

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)

  df <- read_csv2(config$scDataset)
  df <- cleanDataframe(df)
  
  source("location_vs_kwh.R")
  colorData <- CreateDataForMapPlot()
  
  output$table1 <- DT::renderDataTable({
    df
  })
  
  #Time Plots
  output$plot1 <- renderPlot({
    source("time_vs_kwh.R")
    CreatePlotTimeKwh()
  })
  
  output$plot2 <- renderPlot({
    source("smart_charging_vs_kwh.R")
    if(input$SmartVsNotSmart == "smart"){
      CreateBarPlotSmartKwh()
    } else if (input$SmartVsNotSmart == "sessionKwh"){
      CreatePlotSmartKwh1()
    } else if (input$SmartVsNotSmart == "effectiveElapsed"){
      CreatePlotSmartKwh2()
    } else if (input$SmartVsNotSmart == "sessionKwhNS"){
      CreatePlotSmartKwh3()
    } else if (input$SmartVsNotSmart == "effectiveElapsedNS"){
      CreatePlotSmartKwh4()
    }
  })
  
  #Map Plot
  output$map <- renderLeaflet({
    source("location_vs_kwh.R")
    colorData <- CreateDataForMapPlot()
    
    radius <- colorData$total / max(colorData$total) * 300
    pal <- colorBin("plasma", colorData$total, 7, pretty = FALSE)
    
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 4.32, lat = 52.05, zoom = 12) %>% 
      addCircles(
        lng = colorData$longitude,
        lat = colorData$latitude,
        radius = radius, stroke=FALSE, 
        fillOpacity=0.8, color = "#03f",
        fillColor=pal(colorData$total)) %>%
      addLegend("bottomleft", pal=pal, values=colorData$total, title="Total Charged kWh",
                layerId="colorLegend")
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
