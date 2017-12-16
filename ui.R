library(shiny)
library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(plotly)
library(timevis)

source("src/map/map_module.R")
source("src/corrupted_explorer/corrupted_explorer_module.R")

ui <- dashboardPage(
  skin = ("green"),
  dashboardHeader(title = "Social Charging"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "home", selected = TRUE, icon = icon("home")),
      menuItem("Data table", tabName = "table", icon = icon("table")),
      menuItem("Corruption Explorer", tabName = "corruptTab", icon = icon("table")),
      menuItem("Plots", tabName = "charts", icon = icon("bar-chart"),
               menuSubItem("Effective charging", tabName = "chart1"),
               menuSubItem("Weekly charging behaviour", tabName = "chart3"),
               menuSubItem("Daily charging behaviour", tabName = "chart8"),
               menuSubItem("Sessions per timeframe", tabName = "chart4"),
               menuSubItem("Car distributions", tabName = "chart7"),
               menuSubItem("Smart charging vs Non-smart", tabName = "chart2")
      ),
      menuItem("Prediction Plots", tabName = "pred-charts", icon = icon("bar-chart"),
               menuSubItem("Correlation", tabName = "predtab5"),
               menuSubItem("User class distribution", tabName = "predtab1"),
               menuSubItem("Station class distribution", tabName = "predtab7"),
               menuSubItem("User clustering", tabName = "predtab2"),
               menuSubItem("Station clustering", tabName = "predtab6")
      ),
      menuItem("Map", tabName = "mapTab", icon = icon("globe")),
      menuItem("Calendar", tabName = "calendar", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(width = 6, align = "center",
                    h2("Welcome to our presentation"),
                    img(src = "Social-Charging-sheet-smaller.jpg", width = "100%", height = "100%")
                )
              )
      ),
      tabItem(
        tabName = "table",
        fluidRow(
          box(
            title = "Social Charging dataset", status = "success", solidHeader = TRUE, width = 12,
            div(style = "overflow-x: scroll", withSpinner(dataTableOutput("table1")), type = 4)
          )
        )
      ),
      tabItem(tabName = "chart1",
              fluidRow(box(withSpinner(plotOutput("plot1", height = 400,
                                                  dblclick = "dblclick",
                                                  brush = brushOpts(
                                                    id = "brush",
                                                    resetOnNew = TRUE
                                                  )), type = 4), width = 12)),
              actionButton("reset_input", "Reset")
      ),
      tabItem(tabName = "chart2",
              fluidRow(
                box(withSpinner(plotOutput("plot2", height = 750), type = 4), width = 12, height = 800)
              )
      ),
      tabItem(tabName = "chart3",
              fluidRow(
                box(withSpinner(plotOutput("plot3"), type = 4), width = 12)
              )
      ),
      tabItem(tabName = "chart4",
              fluidRow(
                fluidRow(
                  box(withSpinner(plotOutput("plot4", height = 700), type = 4), width = 12, height = 750)
                )
              )
      ),
      tabItem(tabName = "chart7",
              fluidRow(
                box(
                  selectInput(inputId = "plot7Input",
                              label = "Select a chart",
                              choices = c("Car distribution" = "0",
                                          "Average charged kWh per car" = "1"
                              )
                  )
                )
              ),
              fluidRow(
                box(
                  withSpinner(plotOutput("plot7", height = 700), type = 4),
                  width = 12, height = 750)
              )
      ),
      tabItem(tabName = "chart8",
              fluidRow(
                box(withSpinner(plotOutput("plot8", height = 700), type = 4), width = 12, height = 750)
              )
      ),
      tabItem(tabName = "chart9",
              fluidRow(
                box(withSpinner(plotOutput("plot9"), type = 4), width = 12)
              )
      ),
      tabItem(tabName = "chart10",
              fluidRow(
                box(withSpinner(plotOutput("plot10"), type = 4), width = 12)
              )
      ),
      # Pred plots ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "predtab1",
              fluidRow(
                box(withSpinner(plotOutput("pred1"), type = 4), width = 12)
              )
      ),
      tabItem(tabName = "predtab2",
              fluidRow(
                box(withSpinner(plotlyOutput("pred2", height = 700), type = 4), width = 12, height = 750)
              )
      ),
      tabItem(tabName = "predtab5",
              fluidRow(
                box(uiOutput("corColumns")),
                box(withSpinner(plotOutput("cor1", height = 700), type = 4), width = 12, height = 750)
              )
      ),
      tabItem(tabName = "predtab6",
        fluidRow(
          box(withSpinner(plotlyOutput("pred6", height = 700), type = 4), width = 12, height = 750)
        )
      ),
      tabItem(tabName = "predtab7",
              fluidRow(
                box(withSpinner(plotOutput("pred7"), type = 4), width = 16)
              ),
              fluidRow(
                #HLL  	MarriedToThisStation +
                # HHL 	ParkingSpace +
                #HHH	LadyOfTheEvening +
                #LLL	ForeverAlone +
                #LLH	PowerBank +
                #LHH    WorkerBee +
                #LHL	HitAndRun +
                #HLH    LateNightCharging +
                #      occPoint              userPoint                Charge point

                valueBox("MarriedToThisStation", "High occupancy, Low user amount, Low charging amount", icon = icon("list"), color = "green"),
                valueBox("ParkingSpace", "High occupancy, High user amount, Low charging amount", icon = icon("list"), color = "green"),
                valueBox("LadyOfTheEvening", "High occupancy, High user amount, High charging amount", icon = icon("list"), color = "green"),
                valueBox("ForeverAlone", "Low occupancy, Low user amount, Low charging amount", icon = icon("list"), color = "green"),
                valueBox("PowerBank", "Low occupancy, Low user amount, High charging amount", icon = icon("list"), color = "green"),
                valueBox("WorkerBee", "Low occupancy, High user amount, High charging amount", icon = icon("list"), color = "green"),
                valueBox("HitAndRun", "Low occupancy, High user amount, Low charging amount", icon = icon("list"), color = "green"),
                valueBox("LateNightCharging", "High occupancy, Low user amount, High charging amount", icon = icon("list"), color = "green")
              )
      ),
      # Map -------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "mapTab",
              withSpinner(mapModuleUI(id = "map"))
      ),
      tabItem(tabName = "corruptTab",
              corruptedExplorerModuleUI(id = "corrupt")
      ),
      tabItem(tabName = "calendar",
              fluidRow(
                tags$head(
                  includeCSS("src/week_schedule/timvisCss.css")
                ),
                box(withSpinner(htmlOutput("timeline"), type = 4), width = 12)
              ),
              fluidRow(
                  box(
                      title = "select Data", status = "success", solidHeader = TRUE,
                      textInput("text", h4("User Id"), value = "Enter userId number"),
                      dateRangeInput("dates", h4("Period")),
                      br(),
                      actionButton("action", Position = "right", "Action"), width = 6),
                  box(
                      title = "schedule data", status = "success", solidHeader = TRUE,
                      withSpinner(tableOutput("table2")), type = 4, width = 6)
                )
      )
    )
  )
)
