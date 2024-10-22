# Define the server
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(hms)
library(plotly)
library(htmltools)
library(shinyWidgets)
library(reticulate)
library(stringr)
library(purrr)



server <- function(input, output, session) {
  source("Server_Logic_Chess.R")
  combined_df <- reactiveVal(NULL)
  result_df<-reactiveVal(NULL)
  merged_data_complete2<-reactiveVal(NULL)
  
  
  observeEvent(input$getDataBtn, {
    chess_username <- input$username
    desired_rows <- input$numberofgames
    game_types <- input$ChessGameType
    api_url <- paste0("https://api.chess.com/pub/player/", chess_username, "/games/archives")
    
    # Fetch and parse archive URLs
    archives <- tryCatch({
      fromJSON(content(GET(api_url), "text"))
    }, error = function(e) {
      showNotification("Error fetching data. Please check the username or try again later.", type = "error")
      return(NULL)
    })
    
    # Exit if the archives retrieval failed
    req(archives)
    
    # Initialize progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Fetching data", value = 0)
    on.exit(progress$close())
    
    # Fetch and combine game data from archives in reverse order
    archive_data <- list()
    total_archives <- length(archives$archives)
    games_collected <- 0
    
    for (i in rev(seq_along(archives$archives))) {
      archive_games <- fromJSON(content(GET(archives$archives[i]), "text", flatten = TRUE))
      
      # Filter games based on conditions
      filtered_games <- archive_games$games %>%
        filter(grepl(paste(game_types, collapse = "|"), time_class, ignore.case = TRUE),
               rules == "chess", 
               rated == TRUE)
      
      # Combine games if they meet criteria
      if (nrow(filtered_games) > 0) {
        archive_data[[length(archive_data) + 1]] <- filtered_games
        games_collected <- games_collected + nrow(filtered_games)
      }
      
      progress$inc(1 / total_archives, detail = paste("Processing archive", total_archives - i + 1, "of", total_archives))
      
      # Stop if enough games have been collected
      if (games_collected >= desired_rows) {
        break
      }
    }
    
    # Combine collected game data into a single dataframe
    combined_df <- bind_rows(archive_data) %>%
      filter(nrow(.) > 0) # Ensure that there is data after filtering
    combined_df(combined_df)
    # Process each game to extract PGN values and metadata
    result_list <- lapply(seq_len(nrow(combined_df)), function(i) {
      pgn_values <- extract_pgn_values(combined_df$pgn[i])
      pgn_values <- as.data.frame(t(pgn_values), stringsAsFactors = FALSE)
      pgn_values$ChessGameType <- combined_df$time_class[i]
      return(pgn_values)
    })
    
    result_df <- bind_rows(result_list) %>% na.omit()
    
    # Limit the result to the desired number of rows
    if (desired_rows < nrow(result_df)) {
      result_df <- tail(result_df, desired_rows)
    }
    
    result_df$Date <- as.Date(clean_dates(result_df$Date))
    result_df$Result <- sapply(result_df$Result, clean_results)
    result_df$elo_change <- NA
    result_df$total_elo_change <- NA
    
    # Initialize variables to track cumulative Elo change
    last_elos_by_type <- list()
    cumulative_elo_change_by_year_type <- list()
    
    # Iterate through each game to calculate elo_change and total_elo_change
    for (i in 1:nrow(result_df)) {
      game_type <- result_df[i, "ChessGameType"]
      white_player <- result_df[i, "White"]
      black_player <- result_df[i, "Black"]
      white_elo <- as.integer(result_df[i, "WhiteElo"])
      black_elo <- as.integer(result_df[i, "BlackElo"])
      game_year <- format(result_df[i, "Date"], "%Y")
      
      if (is.null(last_elos_by_type[[game_type]])) last_elos_by_type[[game_type]] <- NA
      if (is.null(cumulative_elo_change_by_year_type[[game_year]])) cumulative_elo_change_by_year_type[[game_year]] <- list()
      if (is.null(cumulative_elo_change_by_year_type[[game_year]][[game_type]])) cumulative_elo_change_by_year_type[[game_year]][[game_type]] <- 0
      
      if (white_player == chess_username) {
        current_elo <- white_elo
        opponent_elo <- black_elo
      } else if (black_player == chess_username) {
        current_elo <- black_elo
        opponent_elo <- white_elo
      } else {
        next
      }
      
      if (is.na(last_elos_by_type[[game_type]])) {
        result_df[i, "elo_change"] <- 0
      } else {
        previous_elo <- last_elos_by_type[[game_type]]
        result_df[i, "elo_change"] <- current_elo - previous_elo
      }
      
      cumulative_elo_change_by_year_type[[game_year]][[game_type]] <- cumulative_elo_change_by_year_type[[game_year]][[game_type]] + result_df[i, "elo_change"]
      result_df[i, "total_elo_change"] <- cumulative_elo_change_by_year_type[[game_year]][[game_type]]
      last_elos_by_type[[game_type]] <- current_elo
    }
    
    # Group and summarize data
    result_df <- result_df %>%
      group_by(Date) %>%
      mutate(total_elo_change = last(total_elo_change)) %>%
      ungroup() %>%
      mutate(
        day_of_week = factor(weekdays(Date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
        hour = hour(Date),
        game_number = row_number()
      ) %>%
      select(-Termination, -Link)
    
    result_df$year <- year(result_df$Date)
    result_df$week <- week(result_df$Date)
    result_df$month <- month(result_df$Date)
    
    result_df(result_df)
    # Create summary data
    summary_week <- reactive({
      result_df %>%
        group_by(year, week, ChessGameType) %>%
        summarize(
          mean_week_elo = mean(total_elo_change, na.rm = TRUE),
          sd_week_elo = sd(total_elo_change, na.rm = TRUE),
          count = n(),
          sem_week_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
        )
    })
    
    summary_month <- reactive({
      result_df %>%
        group_by(year, month, ChessGameType) %>%
        summarize(
          mean_month_elo = mean(total_elo_change, na.rm = TRUE),
          sd_month_elo = sd(total_elo_change, na.rm = TRUE),
          count = n(),
          sem_month_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
        )
    })
    
    summary_year <- reactive({
      result_df %>%
        group_by(year, ChessGameType) %>%
        summarize(
          mean_year_elo = mean(total_elo_change, na.rm = TRUE),
          sd_year_elo = sd(total_elo_change, na.rm = TRUE),
          count = n(),
          sem_year_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
        )
    })
    
    # Update UI selections based on available data
    updateSelectInput(session, "MonthSelect", choices = unique(result_df$month), selected = unique(result_df$month))
    updateSelectInput(session, "YearSelect", choices = unique(result_df$year), selected = unique(result_df$year))
    updateSelectInput(session, "WeekSelect", choices = unique(result_df$week), selected = unique(result_df$week))
    
    output$yearSelectUI <- renderUI({
      req(result_df())
      selectInput("YearSelect", "Select Year(s):", choices = unique(result_df()$year), selected = NULL, multiple = TRUE)
    })
    
    output$monthSelectUI <- renderUI({
      req(result_df())
      selectInput("MonthSelect", "Select Month(s):", choices = unique(result_df()$month), selected = unique(result_df()$month), multiple = TRUE)
    })
    
    output$weekSelectUI <- renderUI({
      req(result_df())
      selectInput("WeekSelect", "Select Week(s):", choices = unique(result_df()$week), selected = unique(result_df()$week), multiple = TRUE)
    })
    
    
    
    # Render plots
    
    # Ensure that WhiteElo and BlackElo are numeric
    result_df <- result_df %>%
      mutate(
        WhiteElo = as.numeric(unlist(WhiteElo)),
        BlackElo = as.numeric(unlist(BlackElo))
      )
    
    # Replace 'username' with the actual user input
    username <- input$username 
    
    # Create the Elo value based on whether the user is White or Black
    result_df <- result_df %>%
      mutate(current_elo = case_when(
        White == username ~ WhiteElo,   # If the user is playing White, use WhiteElo
        Black == username ~ BlackElo,   # If the user is playing Black, use BlackElo
        TRUE ~ NA_real_                 # NA for games not involving the user
      ))
    
    output$plotOutput <- renderPlot({
      # Get the maximum game number
      max_game_number <- max(result_df$game_number, na.rm = TRUE)
      
      # Determine the breaks to achieve roughly 4 increments
      interval <- ceiling(max_game_number / 4)  # Divide max by 4 and round up
      
      # Create breaks ensuring at least 4 breaks are shown
      breaks <- seq(0, max_game_number, by = interval)
      
      # Make sure to include the max_game_number if it's not already in the breaks
      if (tail(breaks, 1) < max_game_number) {
        breaks <- c(breaks, max_game_number)
      }
      
      # Now create the g0gplot using date instead of game number
      ggplot(result_df, aes(x = Date, y = current_elo, color = ChessGameType)) +
        geom_point() +
        geom_smooth() +
        labs(title = "Elo Rating Over Date", x = "Date", y = "Current Elo Rating") +
        theme_minimal()
      
      
    })
    
    # Plot the month-over-month Elo change
    # Select the last value of total_elo_change for each ChessGameType and month
    result_df_last <- result_df %>%
      group_by(ChessGameType, month) %>%
      summarise(last_elo_change = tail(total_elo_change, 1)) %>%
      ungroup()
    
    # Plot the last Elo change value for each month and ChessGameType
    output$plotOutput2 <- renderPlot({
      ggplot(result_df_last, aes(x = as.factor(month), y = last_elo_change, fill = ChessGameType)) +
        geom_col(position = position_dodge()) +
        labs(title = "Elo Change for Each Month", x = "Month", y = "Last Elo Change")
    })
    
    # Create the plot with date on the x-axis
    output$plotOutput3 <- renderPlot({
      ggplot(result_df, aes(x = Date, y = total_elo_change, color = ChessGameType)) +
        geom_point() +
        geom_smooth() +
        labs(title = "Elo Change Over Date", x = "Date", y = "Total Elo Change") +
        theme_minimal()
    })
    
    
    output$plotOutput6 <- renderPlotly({
    
    req(input$YearSelect)
    filtered_summary_year <- summary_year() %>%
        filter(year %in% input$YearSelect, !is.na(mean_year_elo))  # Remove NA values
    
    plot_ly(filtered_summary_year, 
            x = ~year, 
            y = ~mean_year_elo, 
            type = "bar", 
            color = ~ChessGameType) %>%
        layout(title = "Yearly Elo Change", 
               xaxis = list(title = "Year"), 
               yaxis = list(title = "Mean Elo"),
               barmode = "group")
})

output$plotOutput4 <- renderPlotly({
    req(input$MonthSelect)
    filtered_summary_month <- summary_month() %>%
        filter(month %in% input$MonthSelect, !is.na(mean_month_elo))  # Remove NA values
    
    plot_ly(filtered_summary_month, 
            x = ~month, 
            y = ~mean_month_elo, 
            type = "bar", 
            color = ~ChessGameType) %>%
        layout(title = "Monthly Elo Change", 
               xaxis = list(title = "Month"), 
               yaxis = list(title = "Mean Elo"),
               barmode = "group")
})

output$plotOutput5 <- renderPlotly({
    req(input$WeekSelect)
    filtered_summary_week <- summary_week() %>%
        filter(week %in% input$WeekSelect, !is.na(mean_week_elo))  # Remove NA values
    
    plot_ly(filtered_summary_week, 
            x = ~week, 
            y = ~mean_week_elo, 
            type = "bar",                # Change type to "bar"
            color = ~ChessGameType) %>%
      layout(title = "Weekly Elo Change", 
             xaxis = list(title = "Week"), 
             yaxis = list(title = "Mean Elo"),
             barmode = "group")
    
})

  })
  
  observeEvent(input$GetTimeBtn, {
    engine_depth <- as.integer(input$EngineDepth)
    py$engine_depth <- engine_depth
    
    py_run_string("
import chess
import chess.engine
engine_depth = 1
move_scores = []

def analyze_each_move(moves_str, depth=engine_depth, stockfish_path='/path/to/stockfish'):
    with chess.engine.SimpleEngine.popen_uci(stockfish_path) as engine:
        board = chess.Board()
        moves = moves_str.split()
        for move in moves:
            board.push_san(move)
            info = engine.analyse(board, chess.engine.Limit(depth=depth))
            
            mate_in = info['score'].relative.mate()
            if mate_in is not None:
                if mate_in == 0:
                    score = 100  # Checkmate
                else:
                    score = 100 * (mate_in / abs(mate_in))
            else:
                score = info['score'].relative.score() / 100  # Normalize centipawn score
            
            if board.turn == chess.BLACK:
                score = -score
            
            move_scores.append((move, score))
        return move_scores
    ")
    
    source("Server_Logic_Chess.R")
    
    combined_df <- combined_df()
    progress <- shiny::Progress$new()
    progress$set(message = "Processing games", value = 0)
    on.exit(progress$close())
    
    selected_time_class <- input$timeClass
    num_games_to_plot <- input$numGames
    
    filtered_data <- combined_df %>%
      filter(rules == "chess", rated == TRUE) %>%
      tail(num_games_to_plot)
    
    total_moves <- sum(sapply(filtered_data$pgn, function(pgn) {
      length(str_extract_all(pgn, "\\d+:\\d+(?:\\.\\d+)?")[[1]])
    }))
    
    ### Stockfish Analysis Section (Python Integration) ###
    
    pgn_list <- strsplit(filtered_data$pgn, "\n\n")
    all_moves <- vector("list", length(pgn_list))
    all_timestamps <- vector("list", length(pgn_list))
    
    for (i in seq_along(pgn_list)) {
      game_pgn <- pgn_list[[i]]
      temp_aa <- extract_moves_and_timestamps(game_pgn)
      temp_aa <- separate_moves_and_timestamps(temp_aa)
      all_moves[[i]] <- temp_aa$moves
      all_timestamps[[i]] <- temp_aa$timestamps
    }
    
    combined_data <- vector("list", length(all_moves))
    for (i in seq_along(all_moves)) {
      moves <- all_moves[[i]]
      timestamps <- all_timestamps[[i]]
      min_length <- min(length(moves), length(timestamps))
      cleaned_timestamps <- clean_timestamp(timestamps[1:min_length])
      
      game_data <- data.frame(
        Move = moves[1:min_length],
        Timestamp = cleaned_timestamps,
        stringsAsFactors = FALSE
      )
      combined_data[[i]] <- game_data
    }
    
    combined_data <- lapply(combined_data, function(game_data) {
      game_data$Move <- clean_moves(game_data$Move)
      game_data
    })

    
    # Initialize result storage for each game
    all_timepermove_player1 <- list()
    all_timepermove_player2 <- list()
    
    # Processing move times
    for (i in seq_along(combined_data)) {
      timestamps <- combined_data[[i]]$Timestamp
      total_time <- sapply(timestamps, convert_timestamp_to_seconds)
      
      # Variables for time tracking for the current game
      timepermove_player1 <- c()
      timepermove_player2 <- c()
      
      # Main processing loop
      for (move in 1:(length(total_time) - 2)) { 
        if (is.na(total_time[move]) || is.na(total_time[move + 2])) {
          next  # Skip this iteration if any timestamp is NA
        }
        
        # Calculate time spent
        time_spent <- round(total_time[move] - total_time[move + 2], 2)
        
        if (time_spent < 0) {
          cat("Negative time detected for move:", move, 
              "Time spent:", time_spent, 
              "Player:", ifelse(move %% 2 == 1, "Player 1 (White)", "Player 2 (Black)"), 
              "\n")
        }
        
        if (move %% 2 == 1) {
          # Player 1 (White)
          timepermove_player1 <- c(timepermove_player1, time_spent)  # Convert to tenths
        } else {
          # Player 2 (Black)
          timepermove_player2 <- c(timepermove_player2, time_spent)  # Convert to tenths
        }
      }
      
      # Store results for this game
      all_timepermove_player1[[i]] <- timepermove_player1
      all_timepermove_player2[[i]] <- timepermove_player2
    }
    
    # Combine time data into a single data frame
    zzztest <- do.call(rbind, lapply(seq_along(all_timepermove_player1), function(list_num) {
      player1_times <- all_timepermove_player1[[list_num]]
      player2_times <- all_timepermove_player2[[list_num]]
      
      combined_times <- c(player1_times, player2_times)
      move_nums <- seq_along(combined_times)
      
      data.frame(
        time_per_move = combined_times,
        move_number = move_nums,
        game_number = list_num,
        time_class = rep(filtered_data$time_class[list_num], length(combined_times))
      )
    }))
    
    # Define the path to the Stockfish engine executable
    stockfish_path <- "stockfish-windows-x86-64-avx2"
    
    # Initialize an empty data frame for storing move and score information
    results_df_scorea <- data.frame(Move = character(), Score = numeric(), stringsAsFactors = FALSE)
    
    # Initialize an empty data frame for storing game results
    game_results_df <- data.frame()
    
    # Loop through each game to analyze the moves
    for (i in 1:num_games_to_plot) {
      # Concatenate all the moves of the current game into a single string
      moves_string <- paste(unlist(combined_data[[i]][["Move"]]), collapse = " ")
      
      # Analyze each move using the Stockfish engine and retrieve scores for each move
      aaresults <- py$analyze_each_move(moves_string, depth = engine_depth, stockfish_path = stockfish_path)
      
      # Convert the analysis results to a data frame
      current_game_df <- as.data.frame(do.call(rbind, aaresults), stringsAsFactors = FALSE)
      
      # Add a game ID column to the current game data
      current_game_df$game_id <- i
      
      # Remove any duplicate rows from the current game's results
      unique_game_df <- current_game_df[!duplicated(current_game_df), ]
      
      # Append the results to the overall game results data frame
      game_results_df <- rbind(game_results_df, unique_game_df)
    }
    
    # Rename the columns of the current game data frame to "Move" and "Score"
    names(current_game_df) <- c("Move", "Score")
    
    # Append the current game data frame to the results data frame
    results_df_scorea <- rbind(results_df_scorea, current_game_df)
    
    # Take a subset of `zzztest` based on the smaller number of rows between `zzztest` and `results_df_scorea`
    zzztest_subset <- zzztest[1:min(nrow(zzztest), nrow(results_df_scorea)), ]
    
    # Match the number of rows between `zzztest` and `results_df_scorea`
    results_df_scorea <- results_df_scorea[1:min(nrow(results_df_scorea), nrow(zzztest)), ]
    
    # Combine `zzztest_subset` and `results_df_scorea` into a single data frame
    merged_data_complete <- cbind(zzztest_subset, results_df_scorea)
    
    # Assign usernames based on the game number, alternating between black and white players
    usernames <- ifelse(seq_along(merged_data_complete$game_number) %% 2 == 1,
                        combined_df$black$username[merged_data_complete$game_number],
                        combined_df$white$username[merged_data_complete$game_number])
    
    # Add the usernames to the merged data and convert the Score column to numeric
    merged_data_complete2 <- cbind(data.frame(username = usernames, stringsAsFactors = FALSE), merged_data_complete)
    merged_data_complete2$Score <- as.numeric(merged_data_complete2$Score)
    
    # Flip the sign of the scores for even moves (i.e., moves by Black)
    even_positions <- seq(2, length(merged_data_complete2$Score), by = 2)
    merged_data_complete2$Score[even_positions] <- merged_data_complete2$Score[even_positions] * -1
    
    # Filter the data to only include rows where the username matches the input username
    merged_data_complete2 <- merged_data_complete2[merged_data_complete2$username == input$username, ]
    merged_data_complete2(merged_data_complete2)
    # Plot 1: Time spent per move with a smoothed line
    output$TimePlotOutput <- renderPlot({
      ggplot(data = zzztest, aes(x = move_number, y = time_per_move, color = time_class)) +
        geom_smooth() +
        ylab("Time Spent Per Move (Seconds)") +
        xlab("Move Number") +
        theme_minimal()
    })
    
    # Plot 2: Bubble plot showing move number, evaluation score, time spent, and players
    output$TimePlotOutput2 <- renderPlot({
      ggplot(merged_data_complete2, aes(x = move_number, y = Score, size = time_per_move, color = username)) +
        geom_point(alpha = 0.5) +
        ylim(-10, 10) +
        labs(x = "Move Number", y = "Evaluation Score", size = "Time Spent (s)", color = "Player") +
        ggtitle("Bubble Plot of Move Number and Evaluation Score with Time Spent and Players") +
        theme_minimal()
    })
    
    # Data table: Display a table of move number, username, score, and time spent per move
    output$TimeTableOutput <- renderDataTable({
      table_data <- merged_data_complete2[, c("move_number", "username", "Score", "time_per_move")]
      datatable(table_data, options = list(pageLength = 10))
    })
    
  })
  
  
  observeEvent(input$GetForecastBtn, {
    
    result_df <- result_df()
    merged_data_complete2 <- merged_data_complete2()
    merged_data_complete2 <- merged_data_complete2[, -ncol(merged_data_complete2)]

    # First, merge result_df and merged_data_complete2
    result_df <- left_join(result_df, merged_data_complete2, by = "game_number")
    # Then slice the result_df to match the number of rows in merged_data_complete2
    result_df <- result_df %>%
      slice(1:nrow(merged_data_complete2))
    username <- input$username
    # Apply all mutations in one pipeline
    result_df2 <- result_df %>%
      mutate(
        current_elo = case_when(
          White == username ~ as.numeric(WhiteElo),    # If the user is playing White, use WhiteElo
          Black == username ~ as.numeric(BlackElo),    # If the user is playing Black, use BlackElo
          TRUE ~ NA_real_                               # NA for games not involving the user
        ),
        current_elo = ifelse(is.na(current_elo),
                             ifelse(White == username, as.numeric(WhiteElo), as.numeric(BlackElo)),
                             current_elo),
        
        opponentElo = case_when(
          White != username ~ as.numeric(WhiteElo),    # If the user is playing Black, opponent is White
          Black != username ~ as.numeric(BlackElo),    # If the user is playing White, opponent is Black
          TRUE ~ NA_real_                               # NA for games not involving the user
        ),
        game_result_numeric = case_when(
          Result == "1-0" ~ 1,             # User wins
          Result == "0-1" ~ 0,             # User loses
          Result == "1/2-1/2" ~ 0.5,       # Draw
          TRUE ~ NA_real_                  # Handle unexpected cases
        )
      )
    
    # Extract features (X) and target (y)
    result_df2$Score <- as.numeric(result_df2$Score)
    result_df2$Score <- as.numeric(result_df2$year)
    result_df2$Score <- as.numeric(result_df2$month)
    result_df2$Score <- as.numeric(result_df2$week)
    
    X <- as.matrix(result_df2[, c("opponentElo", "game_result_numeric", "time_per_move", "move_number", "Score", "game_number", "year", "week", "month")])
    y_true <- result_df2$current_elo
    
    # Define the fitness function (e.g., minimize MSE between predicted Elo and actual Elo)
    fitness_function <- function(params, X, y_true) {
      y_pred <- X %*% params
      mse <- mean((y_true - y_pred)^2)
      return(-mse)  # Return the negative because GA maximizes by default
    }
    
    # Run the genetic algorithm to find the optimal weights
    ga_model <- ga(
      type = "real-valued",
      fitness = function(params) fitness_function(params, X, y_true),
      lower = rep(-1, ncol(X)),
      upper = rep(1, ncol(X)),
      popSize = 5000,
      maxiter = 100000,
      run = 50,
      pmutation = 0.25
    )
    
    # Initialize the current state (last known values)
    current_state <- as.numeric(result_df2[nrow(result_df2), c("opponentElo", "game_result_numeric", "time_per_move", "move_number", "Score", "game_number", "year", "week", "month")])
    
    # Function to predict future Elo
    predict_future_elo <- function(current_state, weights, iterations) {
      predicted_elos <- numeric(iterations)
      
      for (i in 1:iterations) {
        predicted_elo <- sum(current_state * weights)
        predicted_elos[i] <- predicted_elo
        
        current_state[1] <- current_state[1] + rnorm(1, mean = 0, sd = 10)  # Simulate opponentElo change
        current_state[4] <- current_state[4] + 1  # Increment Move_Number.y
        current_state[6] <- current_state[6] + 1  # Increment GameNumber.y
      }
      
      return(predicted_elos)
    }
    best_weights <- ga_model@solution
    future_elos <- predict_future_elo(current_state, best_weights, 10)
    
    # Extract best fitness values as a numeric vector
    best_fitness_values <- ga_model@summary[, 1]  # Ensure this is a vector
    
    # Create a data frame for plotting, transposing the vector to ensure it becomes a row
    fitness_plot_data <- data.frame(
      Iteration = 1:length(best_fitness_values),
      BestFitness = t(best_fitness_values)  # Transpose to make it a row
    )
    

    fitness_plot_data <- data.frame(
      Iteration = 1:length(best_fitness_values),
      BestFitness = best_fitness_values
    )
    
    # Plotting the improvement of the GA with best fitness values
    fitness_plot <- ggplot(data = fitness_plot_data, aes(x = Iteration, y = BestFitness)) +
      geom_smooth(se = FALSE, color = "darkblue") +  # Trend line without standard error
      labs(title = "GA Improvement Over Iterations (Best Fitness)",
           x = "Iteration",
           y = "Best Fitness (Negative MSE)") +
      theme_minimal()
    
    # Render the plot
    output$forecastPlot <- renderPlot({
      print(fitness_plot)
    })
    
    output$forecastText <- renderPrint({
      print(future_elos)  # Print the predicted Elo ratings
    })
    
    ######################UNDER CONSTRUCTION #######################################
    
    library(neuralnet)
    
    set.seed(245)
    
    # Split the result_df into training and test datasets
    data_rows <- floor(0.80 * nrow(result_df2))
    train_indices <- sample(c(1:nrow(result_df2)), data_rows)
    
    # Use result_df for training and testing
    train_data <- result_df2[train_indices, ]
    test_data <- result_df2[-train_indices, ]
    browser()
    # Build the neural network model
    model = neuralnet(
      current_elo ~game_result_numeric + opponentElo + Score + move_number +time_per_move +month +week +year,  # Ensure these columns exist in result_df
      data = train_data,
      hidden = c(4,2),  # Hidden layer configuration
      linear.output = FALSE
    )
    
    plot(model,rep = "best")
    
    pred <- predict(model, test_data)
    
    check = as.numeric(test_data$game_result_numeric) == max.col(pred)
    accuracy = (sum(check)/nrow(test_data))*100
    print(accuracy)
    
    browser()
    
  })
  

  

  
  
}

  