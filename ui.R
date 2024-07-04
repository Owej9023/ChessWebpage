#To Do Not easily
#Elo Forecasting Algorithim
#Improve speed of MQI
#Improve accuracy of MQI
#write function to get the number of blunders, mistakes, innaccuracies, and missed checkmates and categorize them.
#performace by time of day
# add advantage captilization graph - how often you win when then engine favors you heavily 
#judge opening (15 moves)
#judge endgame(last 15 moves)
#judge middlegame moves after the first 15 but not moves after move 40? maybe move 60? will have to see how this can be generalized to account for extremely short and long games
#add resoursefullness- aka how often are you able to win/draw when the engine does not favor you heavily. aka +-6
#add suggestions on what they need to do to improve AKA. where they are performing worse than other players at their elo level, ex.time management,# of blunders,advantage captilization
#add function to download their games as a csv,pgn,etc...
#Blunder %
#extract time spent per move as a list
#breakdown time spent per move
#Reset the elo change after the end of every year


#To Do Reasonably Easily
#show different ways they win
#show different ways they lose
#show different ways they draw
#performance by opening
#add in graphs showing how they compare to other players in a similar elo level
#Win % by color
#add time mangement graphs
#make all graphs interactable


#Done
#add in other pages so you can segment the data analysis into different sections

#Fix Elo change 
#implement a system to distinguish the 23 unqiue types of games on chess.com (NOTE Adjusted to change for game type not for the specifics within each game type. So bullet will account for both 1+0 or 1+1)
#Allow for users to put in their username and get output
#added graph functionality
#Get the data into the app
#Created Function to analyze game and return a numeric value
#performance by day of week
#allow for breaking games into different categories based on time format
#add function that allows to select the last x amount of games to analyze
#Improve speed of getting data




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
                 numericInput("numberofgames", "Number of Games:", value = 500000, min = 1),
                 pickerInput("ChessGameType", "Select Game Types:", choices = c("Rapid", "Blitz", "Bullet"), multiple = TRUE),
                 actionButton("getDataBtn", "Get Data")
               ),
               mainPanel(
                 plotOutput("plotOutput"),
                 plotOutput("plotOutput2"),
                 plotOutput("plotOutput3"),
                 plotlyOutput("plotOutput4"),
                 fluidRow(
                   column(6, align = "left", 
                          selectInput("YearSelect", "Select Year(s):", 
                                      choices = unique(result_df$year), 
                                      selected = unique(result_df$year), 
                                      multiple = TRUE)),
                   
                 plotlyOutput("plotOutput6"),
                 fluidRow(
                   column(6, align = "left", 
                          selectInput("WeekSelect", "Select Week(s):", 
                                      choices = unique(result_df$week), 
                                      selected = unique(result_df$week), 
                                      multiple = TRUE))
                ),
                 ),
                 plotlyOutput("plotOutput5"),
                 fluidRow(
                   column(6, align = "left", 
                          selectInput("MonthSelect", "Select Month(s):", 
                                      choices = unique(result_df$month), 
                                      selected = unique(result_df$month), 
                                      multiple = TRUE))
                 )
               )
             )
    ),
    
    # Second tab - Gameplay
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
                 plotOutput("TimePlotOutput")
               )
             )
    ),
    
    
    # Third tab - TBD
    tabPanel("Gameplay")
  )
)
