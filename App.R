# app.R

#Goal MAE = +- 8
#Goal R^2 = 90-98%
#Goal RMSE = +- 12

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(hms)
library(plotly)
library(shinyWidgets)
library(stringr)


# Source the UI and server definitions
source("ui.R")
source("server.R")
source("Server_Logic_Chess.R")

# Run the Shiny app
shinyApp(ui, server)

#Currently In Progress fix whatever made it not work



#fix smooth in the time so that it is only showing time spent by username (Identify player color and then filter out odd and even moves respectively)
#bubble plot should be the change in evaluation and measure if player took a good move vs a bad move aka min score or max score 
#change the opponenets in time graphs so that it is User Vs Opponent
#investigate why and if the timestamps are not correctly getting time from the pgns aka why some are negative
#Adjust the Code so that all necessary data is stored in reactive values
#Make sure that the code does not collect or transform data more than once


#To Do Not easily
#Create a MQI<- #write function to get the number of blunders, mistakes, innaccuracies, and missed checkmates and categorize them.
#Improve speed of MQI
#Improve accuracy of MQI
# add advantage captilization graph - how often you win when then engine favors you heavily 
#judge opening (15 moves)
#judge endgame(last 15 moves)
#judge middlegame moves after the first 15 but not moves after move 40? maybe move 60? will have to see how this can be generalized to account for extremely short and long games
#add resoursefullness- aka how often are you able to win/draw when the engine does not favor you heavily. aka +-6
#add suggestions on what they need to do to improve AKA. where they are performing worse than other players at their elo level, ex.time management,# of blunders,advantage captilization
#Blunder %





#To Do Reasonably Easily
#add function to download their games as a csv,pgn,etc...
#performace by time of day
#show different ways they win
#show different ways they lose
#show different ways they draw
#performance by opening
#add in graphs showing how they compare to other players in a similar elo level
#Win % by color


#Done

#fix the progress bar in time
#add in time_class to neural net
#make sure neural net does not compare games of different time class
#fix the UI on first page
#Segment Server into functions so that everything that is code related is defined elsewhere
#Rewrite parts of code to make it into repeatable functions for readablitiy
#add multiple time mangement graphs
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
#extract time spent per move as a list
#breakdown time spent per move
#Reset the elo change after the end of every year
#fix the loading bars for the time graph
#Elo Forecasting Algorithim ()Maybe use a genetic algorithim trained on randomly selected accounts from all elo ranges
#implement the GA into the server
#updated graphs 456 in data to auto load
#Fix the issue with result_df2 not getting more than 1 game
#fix issue with merged_data getting the number of moves from input and change it to number of games
#double check moves so that it is getting the correct moves for each game with an eval. In results_df_scorea
#fix timepermove not correctly mathing out time
