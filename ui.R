library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(plotly)
library(GA)
library(DT)
ui <- fluidPage(
  # App title
  titlePanel("Chess Game Data"),
  
  # Use tabsetPanel for multiple pages
  tabsetPanel(
    # First tab - Elo Changes
    tabPanel("Elo Changes", 
             sidebarLayout(
               sidebarPanel(
                 textInput("username", "Chess.com Username:"),
                 numericInput("numberofgames", "Number of Games:", value = 500, min = 1),
                 pickerInput("ChessGameType", "Select Game Types:", choices = c("Rapid", "Blitz", "Bullet"), multiple = TRUE, selected = c("Blitz", "Rapid","Bullet")),
                 actionButton("getDataBtn", "Get Data")
               ),
               mainPanel(
                 plotOutput("plotOutput"),
                 br(),  # Add space between plots
                 plotOutput("plotOutput2"),
                 br(),  # Add space between plots
                 plotOutput("plotOutput3"),
                 br(),  # Add space between plots
                 plotlyOutput("plotOutput6"),
                 br(),  # Add space between plots
                 fluidRow(
                   column(6, align = "left", 
                          uiOutput("yearSelectUI")  # Dynamic UI for year selection
                   )
                 ),
                 plotlyOutput("plotOutput4"),
                 br(),  # Add space between plots
                 fluidRow(
                   column(6, align = "left", 
                          uiOutput("monthSelectUI")  # Dynamic UI for month selection
                   )
                 ),
                 br(),  # Add space between plots
                 plotlyOutput("plotOutput5"),
                 br(),  # Add space between plots
                 fluidRow(
                   column(6, align = "left", 
                          uiOutput("weekSelectUI")   # Dynamic UI for week selection
                   )
                 )
               )
             )
    ),
    
    # Second tab - TimeUse
    tabPanel("TimeUse",
             sidebarLayout(
               sidebarPanel(
                 numericInput("numGames", "Number of Games:", min = 1, max = 1000000, value = 10),
                 numericInput("EngineDepth", "Depth of Engine Search", min = 1, max = 100, value = 5),
                 selectInput("timeClass", "Time Class:", 
                             choices = list("Bullet" = "bullet", 
                                            "Blitz" = "blitz", 
                                            "Rapid" = "rapid"),
                             selected = "blitz",
                             multiple = TRUE),
                 actionButton("GetTimeBtn", "Get TimeData")
               ),
               mainPanel(
                 plotOutput("TimePlotOutput"),
                 br(),  # Add space between plots
                 plotOutput("TimePlotOutput2"),
                 br(),
                 dataTableOutput("TimeTableOutput")
               )
             )
    ),
    
    # Third tab - Forecasting and other analytics
    tabPanel("Forecasting",
             sidebarLayout(
               sidebarPanel(
                 actionButton("GetForecastBtn","Perform Elo Forecasting")
               ),
               mainPanel(
                 plotOutput("forecastPlot"),   # Placeholder for forecast plot
                 br(),                         # Add space
                 textOutput("forecastText")    # Placeholder for forecast-related text output
               )
             )
    )
  )
)

