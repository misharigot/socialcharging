library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Social Charging"),
  dashboardSidebar(
    menuItem("Raw data", tabName = "raw", icon = icon("th")),
    menuItem("Charts", tabName = "raw", icon = icon("bar-chart"),
             menuSubItem("Time elapsed vs kWh charged", tabName = "chart1"),
             menuSubItem("Smart vs non-smart charging", tabName = "chart2"),
             menuSubItem("kWh vs charging stations", tabName = "chart3"),
             menuSubItem("Timeframe vs charging sessions", tabName = "chart4"),
             menuSubItem("Total KwH per station in map", tabName="chart5"),
             menuSubItem("How many users use how many stations", tabName="chart6"),
             menuSubItem("Analyzing per Car", tabName = "chart7")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "raw",
        fluidRow(
          box(
            title = "Social Charging dataset", status = "success", solidHeader = TRUE, width = 12,
            div(style = "overflow-x: scroll", dataTableOutput("table1"))
          )
        )
      ),
      tabItem(tabName = "chart1",
              fluidRow(box(plotOutput("plot1", height = 400,
                                      dblclick = "dblclick",
                                      brush = brushOpts(
                                        id = "brush",
                                        resetOnNew = TRUE
                                      )), width = 12)),
              actionButton("reset_input", "Reset")
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
              fluidRow(box(plotOutput("plot2", height = 400,
                                      dblclick = "dblclick",
                                      brush = brushOpts(
                                        id = "brush",
                                        resetOnNew = TRUE
                                      )), title = "Smart charging ", width = 12)),
              actionButton("reset_input_1", "Reset")
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
      ),
      tabItem(tabName = "chart5",
        div(class="outer",
            tags$head(
              # Include our custom CSS
              includeCSS("src/styles.css"),
              includeScript("src/gomap.js")
            ),
            # If not using custom CSS, set height of leafletOutput to a number instead of percent
            leafletOutput("plot5", width="100%", height="100%"),
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          
                          h2("Data Explorer"),
                          
                          selectInput("category", "Category", 
                                      c("kwh", "Popularity", "Efficiency"))
          )
        )
      ),
      tabItem(tabName = "chart6",
              fluidRow(
                box(plotOutput("plot6"), width = 12)
              )
      ),
      tabItem(tabName = "chart7",
              fluidRow(
                box(
                  title = "Controls", width = 5, solidHeader = TRUE, status = "primary",
                  selectInput(inputId = "plot7Input",
                              label = "Select a chart",
                              choices = c("PersantagePerCar" = "0",
                                          "TotaltotalChargingPerCar" = "1"
                              ))
                )
              ),
              fluidRow(box(plotOutput("plot7"), title = "Analyzing per car ", width = 12))
      )
    )
  )
)
