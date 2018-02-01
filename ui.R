library(shiny)
library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(plotly)
library(timevis)
library(shinyBS)

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
               menuSubItem("User class distribution", tabName = "predtab1"),
               menuSubItem("Station class distribution", tabName = "predtab7"),
               menuSubItem("User clustering", tabName = "predtab2"),
               menuSubItem("Station clustering", tabName = "predtab6")
      ),
      menuItem("Correlation Plots", tabName = "predtab5", icon = icon("bar-chart")),
      menuItem("Map", tabName = "mapTab", icon = icon("globe")),
      menuItem("Week Schedule", tabName = "weekschedule", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(width = 6, align = "center",
                    h2("Welcome"),
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
              fluidRow(box(h4("Social Charging dataset"),
                           withSpinner(plotOutput("plot1", height = 400,
                                                  dblclick = "dblclick",
                                                  brush = brushOpts(
                                                    id = "brush",
                                                    resetOnNew = TRUE
                                                  )), type = 4), width = 6),
                       box(h4("HVA dataset"),
                           img(src = "EffectiveChargingHvA.png",
                               width = "100%", height = "100%"),
                           width = 6)
              ),
              actionButton("reset_input", "Reset")

      ),
      tabItem(tabName = "chart2",
              fluidRow(
                box(
                  withSpinner(plotOutput("plot2", height = 750), type = 4), width = 12, height = 800)
              )
      ),
      tabItem(tabName = "chart3",
              fluidRow(
                box(h4("Social Charging dataset"),
                    withSpinner(plotOutput("plot3"), type = 4), width = 6),
                box(h4("HVA dataset"),
                    img(src = "WeeklyChargingBehaviour.png",
                        width = "100%", height = "100%"),
                    width = 6)
              )
      ),
      tabItem(tabName = "chart4",
              fluidRow(
                box(h4("Social Charging dataset"),
                    withSpinner(plotOutput("plot4", height = 700), type = 4), width = 12, height = 750),
                box(h4("HVA dataset"),
                    img(src = "SessionsPerTimeframe.png",
                        width = "100%", height = "100%"),
                    width = 12)
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
                box(h4("Social Charging dataset"),
                    withSpinner(plotOutput("plot8", height = 700), type = 4), width = 12, height = 750),
                box(h4("HVA dataset"),
                    img(src = "DailyChargingBehaviour.png",
                        width = "100%", height = "100%"),
                    width = 12)
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
                box(h4("Social Charging dataset"),
                    withSpinner(plotOutput("pred1"), type = 4), width = 12, align = "center"),
                box(h4("HVA dataset"),
                    img(src = "UserClassification.png",
                        width = "100%", height = "100%"),
                    width = 12, align = "center")
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
                box(h4("Social Charging dataset"),
                    withSpinner(plotOutput("cor1", height = 700), type = 4), width = 12, height = 750, align = "center"),
                box(h4("HVA dataset"),
                    img(src = "CorrelationPlot.png",
                        width = "50%", height = "50%"),
                    width = 12, align = "center")
              )
      ),
      tabItem(tabName = "predtab6",
              fluidRow(
                box(withSpinner(plotlyOutput("pred6", height = 700), type = 4), width = 12, height = 750)
              )
      ),
      tabItem(tabName = "predtab7",
              fluidRow(
                box(h4("Social Charging dataset"),
                    withSpinner(plotOutput("pred7"), type = 4), width = 16, align = "center"),
                box(h4("HVA dataset"),
                    img(src = "StationClassification.png",
                        width = "100%", height = "100%"),
                    width = 12, align = "center")
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
      # Week Schedule ---------------------------------------------------------------------------------------------------
      tabItem(tabName = "weekschedule",
              fluidRow(
                box(selectInput(inputId = "wsUserSelect", label = "Select a user", choices = c("Select a user")),
                    width = 3),
                box(title = "Info",
                    div(
                      tags$p("- The accuracy of the prediction is dependent on the amount of historic data the selected user has.
                             The more data the user has, the more accurate the predictions can be."),
                      tags$p("- When a predicted week has no sessions, then no reliable predictions could be made, due to too little data available for the selected user."),
                      tags$b("Tip: Mouse over a predicted session to see additional information about that session.")
                    ),
                    width = 6
                ),
                box(title = "Legend",
                    div(
                      div(
                        style = "text-align: right;",
                        bsButton("legendInfo", label = "", icon = icon("question"),
                                 style = "default", size = "small"),
                        bsPopover(id = "legendInfo", title = "Explanation of terms",
                                  content = "Efficiency: The efficiency of a predicted session. This is based on the predicted amount of time charging vs idle time (plugged in without actually charging).",
                                  placement = "bottom",
                                  trigger = "focus")
                      ),
                      img(src = "Legend.PNG", width = "50%", height = "50%")
                    ),
                    width = 3
                )
              ),
              fluidRow(
                tags$head(includeCSS("src/css/weekschedule.css")),
                box(withSpinner(timevisOutput("weekschedule", height = '150px')),
                    type = 4 ,
                    width = 12,
                    title = "Predicted week schedule",
                    footer = "This is a representation of the sessions for a future week for the selected user profile. The actual dates showed aren't meaningful and exist just for demonstration purposes.")
              )
      )
    )
  )
)
