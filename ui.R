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

result_df<-c()
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
                 pickerInput("ChessGameType", "Select Game Types:", choices = c("Rapid", "Blitz", "Bullet"), multiple = TRUE,selected = c("Blitz", "Rapid","Bullet")),
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
                          selectInput("YearSelect", "Select Year(s):", 
                                      choices = unique(result_df$year), 
                                      selected = unique(result_df$year), 
                                      multiple = TRUE))
                 ),
                 plotlyOutput("plotOutput4"),
                 br(),  # Add space between plots
                 fluidRow(
                   column(6, align = "left", 
                          selectInput("MonthSelect", "Select Month(s):", 
                                      choices = unique(result_df$month), 
                                      selected = unique(result_df$month), 
                                      multiple = TRUE))
                 ),
                 br(),  # Add space between plots
                 plotlyOutput("plotOutput5"),
                 br(),  # Add space between plots
                 fluidRow(
                   column(6, align = "left", 
                          selectInput("WeekSelect", "Select Week(s):", 
                                      choices = unique(result_df$week), 
                                      selected = unique(result_df$week), 
                                      multiple = TRUE))
                 )
               )
             )
    ),
    
    # Second tab - TimeUse
    tabPanel("TimeUse",
             sidebarLayout(
               sidebarPanel(
                 numericInput("numGames", "Number of Games:", min = 1, max = 100, value = 10),
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
                 plotOutput("TimePlotOutput2")
               )
             )
    ),
    
    # Third tab - TBD
    tabPanel("Gameplay")
  )
)

