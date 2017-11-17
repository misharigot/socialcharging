library(shiny)
library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(plotly)

source("src/map/map_module.R")
source("src/map/regression_map_module.R")

ui <- dashboardPage(
  skin = ("green"),
  dashboardHeader(title = "Social Charging"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "dash", selected = TRUE, icon = icon("home")),
      menuItem("Data table", tabName = "raw", icon = icon("table")),
      menuItem("Plots", tabName = "charts", icon = icon("bar-chart"),
               menuSubItem("Time vs kWh", tabName = "chart1"),
               menuSubItem("Smart vs Non-smart", tabName = "chart2"),
               menuSubItem("kWh vs Stations", tabName = "chart3"),
               menuSubItem("Timeframe vs Sessions", tabName = "chart4"),
               menuSubItem("Analyzing per Car", tabName = "chart7"),
               menuSubItem("Timeframe vs users", tabName = "chart8")
      ),
      menuItem("Prediction Plots", tabName = "pred-charts", icon = icon("bar-chart"),
               menuSubItem("User classification distribution", tabName = "predtab1"),
               menuSubItem("User clustering", tabName = "predtab2"),
               menuSubItem("Session clustering", tabName = "predtab3"),
               menuSubItem("Station clustering", tabName = "predtab6"),
               menuSubItem("Linear model", tabName = "predtab4"),
               menuSubItem("Correlation", tabName = "predtab5")
      ),
      menuItem("Map", tabName = "mapTab", icon = icon("globe")),
      menuItem("Regression Map", tabName = "regressionMapTab", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dash",
              fluidRow(
                box(width = 6, align = "center",
                    h2("Welcome to our presentation"),
                    img(src = "Social-Charging-sheet-smaller.jpg", width = "100%", height = "100%")
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
              fluidRow(box(withSpinner(plotOutput("plot1", height = 400,
                                      dblclick = "dblclick",
                                      brush = brushOpts(
                                        id = "brush",
                                        resetOnNew = TRUE
                                      ))), width = 12)),
              actionButton("reset_input", "Reset")
      ),
      tabItem(tabName = "chart2",
              fluidRow(
                box(withSpinner(plotOutput("plot2", height = 750)), width = 12, height = 800)
              )
      ),
      tabItem(tabName = "chart3",
              fluidRow(
                box(withSpinner(plotOutput("plot3")), width = 12)
              )
      ),
      tabItem(tabName = "chart4",
              fluidRow(
                fluidRow(
                  box(withSpinner(plotOutput("plot4")), width = 12)
                )
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
                              )
                  )
                )
              ),
              fluidRow(
                box(
                  withSpinner(plotOutput("plot7")),
                  title = "Analyzing per car ",
                  width = 12)
              )
      ),
      tabItem(tabName = "chart8",
              fluidRow(
                box(withSpinner(plotOutput("plot8")), width = 12)
              )
      ),
      tabItem(tabName = "chart9",
              fluidRow(
                box(withSpinner(plotOutput("plot9")), width = 12)
              )
      ),
      tabItem(tabName = "chart10",
              fluidRow(
                box(withSpinner(plotOutput("plot10")), width = 12)
              )
      ),
# Pred plots ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "predtab1",
              fluidRow(
                box(withSpinner(plotOutput("pred1")), width = 12)
              )
      ),
      tabItem(tabName = "predtab2",
              fluidRow(
                box(withSpinner(plotlyOutput("pred2")), width = 12)
              )
      ),
      tabItem(tabName = "predtab3",
              fluidRow(
                box(withSpinner(plotlyOutput("pred3")), width = 12)
              )
      ),
      tabItem(tabName = "predtab4",
              fluidRow(
                box(withSpinner(plotOutput("pred4")), width = 12)
              )
      ),
      tabItem(tabName = "predtab5",
              fluidRow(
                box(withSpinner(plotOutput("cor1")), width = 12)
              )
      ),
      tabItem(tabName = "predtab6",
        fluidRow(
          box(withSpinner(plotlyOutput("pred6")), width = 12)
        )
      ),
# Map -------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "mapTab",
              mapModuleUI(id = "map")
      ),
      tabItem(tabName = "regressionMapTab",
        regressionMapModuleUI(id = "regressionMap")
      )
    )
  )
)
