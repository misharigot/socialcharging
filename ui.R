library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Social Charging"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dash", selected = TRUE, icon = icon("dashboard")),
    menuItem("Raw data", tabName = "raw", icon = icon("th")),
    menuItem("Charts", tabName = "charts", icon = icon("bar-chart"),
             menuSubItem("Time elapsed vs kWh charged", tabName = "chart1"),
             menuSubItem("Smart vs non-smart charging", tabName = "chart2"),
             menuSubItem("kWh vs charging stations", tabName = "chart3"),
             menuSubItem("Timeframe vs charging sessions", tabName = "chart4"),
             menuSubItem("Charging stations visualised on map", tabName = "chart5"),
             menuSubItem("How many users use how many stations", tabName="chart6"),
             menuSubItem("Analyzing per Car", tabName = "chart7"),
             menuSubItem("Timeframe vs users", tabName="chart8")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dash",
        fluidRow(
          box(width = 6, align="center",
              h2("Welcome to our presentation"),
              img(src='Social-Charging-sheet-smaller.jpg', width = "100%", height = "100%")
          )
        )
      ),
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
                box(plotOutput("plot2", height = 750), width = 12, height = 800)
              )
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

                          h2("Plot that SHIT"),

                          selectInput("category", "Category",
                                      c("Charged kWh per station", "Occupation percentage",
                                        "Efficiency percentage", "Users per station")),

                          h5("The category determines the size of the circles")
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
                                          "AverageChargedKwhPerCar" = "1"
                              ))
                )
              ),
              fluidRow(
                box(
                  plotOutput("plot7"), 
                  title = "Analyzing per car ", 
                  width = 12)
              )
      ),
      tabItem(tabName = "chart8",
              fluidRow(
                box(plotOutput("plot8"), width = 12)
              )
      )
    )
  )
)

  