

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

# Function to extract values from the PGN string
extract_pgn_values <- function(pgn, num_values = 21) {
  input_text <- paste(pgn, collapse = " ")
  matches <- str_extract_all(input_text, "\\[(.*?)\\]")[[1]][1:num_values]
  return(matches)
}

# Function to clean date values
clean_dates <- function(dates) {
  return(as.Date(gsub("\\[Date \"|\"\\]", "", dates), format = "%Y.%m.%d"))
}

# Function to clean result values
clean_results <- function(results) {
  return(regmatches(results, regexpr("\\d+/\\d+-\\d+/\\d+|\\d+-\\d+", results))[[1]])
}

# Function to clean Elo values
clean_elo <- function(elo) {
  return(as.numeric(gsub("\\D", "", elo)))
}

# Function to clean time values
clean_time <- function(time) {
  return(hms::as_hms(regmatches(time, regexpr("\\d{2}:\\d{2}:\\d{2}", time))[[1]]))
}

# Function to clean usernames
clean_usernames <- function(user_string) {
  return(sub("\\[.*?\"(.*?)\"\\]", "\\1", user_string))
}


server <- function(input, output, session) {
  observeEvent(input$getDataBtn, {
    chess_username <- input$username
    desired_rows <- input$numberofgames
    game_types <- input$ChessGameType
    api_url <- paste0("https://api.chess.com/pub/player/", chess_username, "/games/archives")
    
    # Fetch and parse archive URLs
    archives <- jsonlite::fromJSON(content(GET(api_url), "text"))
    
    # Initialize progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Fetching data", value = 0)
    on.exit(progress$close())
    
    # Fetch and combine game data from all archives
    archive_data <- list()
    
    total_archives <- length(archives$archives)
    for (i in seq_along(archives$archives)) {
      archive_data[[i]] <- jsonlite::fromJSON(content(GET(archives$archives[i]), "text", flatten = TRUE))
      progress$inc(1 / total_archives, detail = paste("Processing archive", i, "of", total_archives))
    }
    
    combined_df <- bind_rows(archive_data) %>%
      filter(grepl(paste(game_types, collapse = "|"), games$time_class, ignore.case = TRUE),
             games$rules == "chess", 
             games$rated == TRUE)
    
    # Extract values from PGN strings and include ChessGameType
    result_list <- lapply(seq_len(nrow(combined_df$games)), function(i) {
      pgn_values <- extract_pgn_values(combined_df$games$pgn[i])
      pgn_values <- as.data.frame(t(pgn_values), stringsAsFactors = FALSE) # Ensure pgn_values is a data frame
      pgn_values$ChessGameType <- combined_df$games$time_class[i]
      return(pgn_values)
    })
    
    result_df <- bind_rows(result_list) %>% na.omit()
    
    # Ensure desired number of rows
    if (desired_rows < nrow(result_df)) {
      result_df <- tail(result_df, desired_rows)
    }
    

    # Clean and transform data
    result_df$V3 <- clean_dates(result_df$V3)
    result_df$V7 <- sapply(result_df$V7, clean_results)
    result_df[, c(14, 15)] <- sapply(result_df[, c(14, 15)], clean_elo)
    result_df$V16 <- sapply(result_df$V16, clean_elo)
    result_df[, c(18, 20)] <- lapply(result_df[, c(18, 20)], clean_time)
    result_df[, c(5, 6)] <- sapply(result_df[, c(5, 6)], clean_usernames)
    
    
    # Initialize columns for elo_change and total_elo_change
    result_df$elo_change <- NA
    result_df$total_elo_change <- NA
    
    # Loop through each game
    for (i in 1:nrow(result_df)) {
      if (result_df[i, "V5"] == chess_username) {
        current_elo <- result_df[i, "V14"]  # Elo for White
        opponent_elo <- result_df[i, "V15"]  # Elo for Black
      } else if (result_df[i, "V6"] == chess_username) {
        current_elo <- result_df[i, "V15"]  # Elo for Black
        opponent_elo <- result_df[i, "V14"]  # Elo for White
      } else {
        next  # Skip if chess_username is not involved in this game
      }
      
      if (i > 1) {
        previous_elo <- ifelse(result_df[i - 1, "V5"] == chess_username, 
                               result_df[i - 1, "V14"], 
                               result_df[i - 1, "V15"])
        elo_change <- current_elo - previous_elo
        result_df[i, "elo_change"] <- elo_change
      }
    }
    
    # Vectorized calculation of total_elo_change
    result_df$total_elo_change <- ave(result_df$elo_change, result_df$V3, FUN = cumsum)
    
    result_df <- result_df %>%
      group_by(V3) %>%
      mutate(total_elo_change = last(total_elo_change)) %>%
      ungroup()
    
    
    result_df <- result_df %>%
      group_by(V3) %>%
      mutate(total_elo_change = last(total_elo_change)) %>%
      ungroup() %>%
      mutate(
        day_of_week = factor(weekdays(V3), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
        chess_username = ifelse(chess_username %in% c(V5, V6), as.numeric(ifelse(chess_username %in% V5, V14, V15)), NA_real_),
        hour = hour(V18),
        hourend = hour(V20),
        game_number = row_number(),
        game_type = result_df$ChessGameType,  # Add game_type column
      ) %>%
      select(-V17, -V21)
    
    # Group and summarize data
    recent_games_data <- result_df
    recent_games_data_games_per_day <- recent_games_data %>%
      group_by(V3) %>%
      mutate(
        total_games_per_day = n(),
        group_id = ceiling(row_number() / 10)
      ) %>%
      ungroup()
    
    summary_data <- recent_games_data_games_per_day %>%
      group_by(day_of_week, game_type = result_df$ChessGameType) %>%
      summarize(
        mean_elo_change = mean(total_elo_change),
        sd_elo_change = sd(total_elo_change),
        count = n(),
        sem_elo_change = sd(total_elo_change) / sqrt(n())
      )
    
    summary_data_week <- recent_games_data %>%
      group_by(week_number = as.numeric(format(V3, "%U")),game_type=result_df$ChessGameType) %>%
      summarize(
        mean_eweek_elo = mean(total_elo_change),
        sd_eweek_elo = sd(total_elo_change),
        count = n(),
        sem_eweek_elo = sd(total_elo_change) / sqrt(n())
      )
    
    summary_data_month <- recent_games_data %>%
      group_by(month = factor(format(V3, "%B"), levels = month.name, ordered = TRUE),game_type=result_df$ChessGameType) %>%
      summarize(
        mean_emon_elo = mean(total_elo_change),
        sd_emon_elo = sd(total_elo_change),
        count = n(),
        sem_emon_elo = sd(total_elo_change) / sqrt(n())
      )
    
    # Render plots
    output$plotOutput <- renderPlot({
      ggplot(result_df, aes(x = game_number, y = ifelse(input$username == V5, V14, V15), color = game_type)) +
        geom_point() +
        geom_smooth(size = 2) +
        labs(title = "Elo over time", x = "Number of games", y = "Elo") +
        theme_minimal()
    })
    
    output$plotOutput2 <- renderPlot({
      ggplot(recent_games_data_games_per_day, aes(y = total_elo_change, x = total_games_per_day, color = game_type)) +
        geom_smooth() +
        geom_point() +
        labs(title = "Plot of Change in Elo across multiple games in a day", x = "Total # of games played in a day", y = "Total Change in Elo")
    })
    output$plotOutput3 <- renderPlot({
      ggplot(summary_data, aes(x = day_of_week, y = mean_elo_change, fill = game_type)) +
        geom_col(position = "dodge") +
        geom_errorbar(aes(ymin = mean_elo_change - sem_elo_change, ymax = mean_elo_change + sem_elo_change), width = 0.4, position = position_dodge(0.9)) +
        labs(title = "Mean Elo Gain/Loss by Day of the Week", x = "Day of the Week", y = "Mean Elo Change") +
        theme_minimal()
    })
    
    output$plotOutput4 <- renderPlotly({
      plot_ly(summary_data_week, x = ~week_number, y = ~mean_eweek_elo, type = "bar", color = ~game_type,
              marker = list(colors = c("#4E79A7", "#E15759", "#76B7B2"))) %>%
        layout(title = "Mean Change in Elo by Week",
               xaxis = list(title = "Week"),
               yaxis = list(title = "Mean Elo"),
               showlegend = TRUE)
    })
    
    output$plotOutput7 <- renderPlotly({
      plot_ly(summary_data_month, x = ~month, y = ~mean_emon_elo, type = "bar", color = ~game_type,
              marker = list(colors = c("#4E79A7", "#E15759", "#76B7B2"))) %>%
        layout(title = "Mean Change in Elo by Month",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Mean Elo"),
               showlegend = TRUE)
    })
    
    output$plotOutput6 <- renderPlot({})
    output$plotOutput5 <- renderPlot({})
  })
}