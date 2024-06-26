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
#implement a system to distinguish the 23 unique types of games on chess.com


#To Do Reasonably Easily
#show different ways they win
#show different ways they lose
#show different ways they draw
#performance by opening
#add in graphs showing how they compare to other players in a similar elo level
#Win % by color
#add time mangement graphs


#Done
#Allow for users to put in their username and get output
#added graph functionality
#Get the data into the app
#Created Function to analyze game and return a numeric value
#performance by day of week
#allow for breaking games into different categories based on time format
#add function that allows to select the last x amount of games to analyze



library(plotly)
library(httr)
library(dplyr)
library(shiny)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(bigchess)

ui <- fluidPage(
  titlePanel("Chess Application"),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Chess.com Username"),
      numericInput("numberofgames", "Number Of Games to Analyze", 1),
      textInput("ChessGameType", "Chess Time Format"),
      actionButton("getDataBtn", "Get Data")
    ),
    mainPanel(
      textOutput("textOutput"),
      plotOutput("plotOutput"),
      plotOutput("plotOutput1"),
      plotOutput("plotOutput2"),
      plotOutput("plotOutput3"),
      plotOutput("plotOutput4"),
      plotOutput("plotOutput5"),
      plotOutput("plotOutput6"),
      plotlyOutput("plotOutput7"),
      
      
    )
  )
)