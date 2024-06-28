# app.R


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

# Run the Shiny app
shinyApp(ui, server)

