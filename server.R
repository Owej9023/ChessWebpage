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
    library(future.apply)
    plan(multisession)  # Parallel processing
    
    engine_depth <- as.integer(input$EngineDepth)
    py$engine_depth <- engine_depth
    
    # Define the Python code with batch analysis capability
    py_run_string("
import chess
import chess.engine
from multiprocessing import Pool

engine_depth = 1
stockfish_path = '/path/to/stockfish'

def analyze_batch_of_moves(moves_str, depth=engine_depth, stockfish_path=stockfish_path):
    with chess.engine.SimpleEngine.popen_uci(stockfish_path) as engine:
        board = chess.Board()
        moves = moves_str.split()
        results = []
        
        for idx, move in enumerate(moves):
            board.push_san(move)
            # Adjust depth based on move number if needed
            move_depth = min(depth, 8 if idx < 10 else depth)  # Example depth adjustment
            info = engine.analyse(board, chess.engine.Limit(depth=move_depth))
            
            mate_in = info['score'].relative.mate()
            if mate_in is not None:
                score = 100 if mate_in == 0 else 100 * (mate_in / abs(mate_in))
            else:
                score = info['score'].relative.score() / 100  # Centipawn to score
            if board.turn == chess.BLACK:
                score = -score
            results.append((move, score))
        return results
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
    
    all_timepermove_player1 <- list()
    all_timepermove_player2 <- list()
    
    for (i in seq_along(combined_data)) {
      timestamps <- combined_data[[i]]$Timestamp
      total_time <- sapply(timestamps, convert_timestamp_to_seconds)
      
      timepermove_player1 <- c()
      timepermove_player2 <- c()
      
      for (move in 1:(length(total_time) - 2)) {
        time_spent <- round(total_time[move] - total_time[move + 2], 2)
        
        if (move %% 2 == 1) {
          timepermove_player1 <- c(timepermove_player1, time_spent)
        } else {
          timepermove_player2 <- c(timepermove_player2, time_spent)
        }
      }
      
      all_timepermove_player1[[i]] <- timepermove_player1
      all_timepermove_player2[[i]] <- timepermove_player2
    }
    
    zzztest <- do.call(rbind, future_lapply(seq_along(all_timepermove_player1), function(list_num) {
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
    
    stockfish_path <- "stockfish-windows-x86-64-avx2"
    
    results_df_scorea <- data.frame(Move = character(), Score = numeric(), stringsAsFactors = FALSE)
    game_results_df <- data.frame()
    
    for (i in 1:num_games_to_plot) {
      moves_string <- paste(unlist(combined_data[[i]][["Move"]]), collapse = " ")
      
      aaresults <- py$analyze_batch_of_moves(moves_string, depth = engine_depth, stockfish_path = stockfish_path)
      current_game_df <- as.data.frame(do.call(rbind, aaresults), stringsAsFactors = FALSE)
      
      current_game_df$game_id <- i
      unique_game_df <- current_game_df[!duplicated(current_game_df), ]
      game_results_df <- rbind(game_results_df, unique_game_df)
      
      if (i %% 10 == 0) {  # Update progress bar every 5 games
        progress$inc(10 / num_games_to_plot, detail = paste("Analyzing game", i, "of", num_games_to_plot))
      }
    }
    
    names(current_game_df) <- c("Move", "Score")
    results_df_scorea <- rbind(results_df_scorea, current_game_df)
    
    # Determine the number of rows in the selected number of games
    num_rows_in_selected_games <- length(zzztest)
    # Subset zzztest based on the minimum of its own rows and the rows in selected games
    zzztest_subset <- zzztest[1:min(nrow(zzztest), num_rows_in_selected_games), ]
    
    results_df_scorea <- data.frame(Move = character(), Score = numeric(), stringsAsFactors = FALSE)
    
    for (i in 1:num_games_to_plot) {
      moves_string <- paste(unlist(combined_data[[i]][["Move"]]), collapse = " ")
      
      aaresults <- py$analyze_batch_of_moves(moves_string, depth = engine_depth, stockfish_path = stockfish_path)
      current_game_df <- as.data.frame(do.call(rbind, aaresults), stringsAsFactors = FALSE)
      names(current_game_df) <- c("Move", "Score")
      
      # Append data from each game to results_df_scorea
      results_df_scorea <- rbind(results_df_scorea, current_game_df)
      
      if (i %% 10 == 0) {  # Update progress bar every 10 games
        progress$inc(10 / num_games_to_plot, detail = paste("Analyzing game", i, "of", num_games_to_plot))
      }
    }
    
    # Now limit results_df_scorea to match the rows in zzztest
    results_df_scorea <- results_df_scorea[1:nrow(zzztest), ]
    
    merged_data_complete <- cbind(zzztest, results_df_scorea)
    usernames <- ifelse(seq_along(merged_data_complete$game_number) %% 2 == 1,
                        combined_df$black$username[merged_data_complete$game_number],
                        combined_df$white$username[merged_data_complete$game_number])
    
    merged_data_complete2 <- cbind(data.frame(username = usernames, stringsAsFactors = FALSE), merged_data_complete)
    merged_data_complete2$Score <- as.numeric(merged_data_complete2$Score)
    
    even_positions <- seq(2, length(merged_data_complete2$Score), by = 2)
    merged_data_complete2$Score[even_positions] <- merged_data_complete2$Score[even_positions] * -1
    
    merged_data_complete2 <- merged_data_complete2[merged_data_complete2$username == input$username, ]
    merged_data_complete2(merged_data_complete2)
    
    output$TimePlotOutput <- renderPlot({
      ggplot(data = zzztest, aes(x = move_number, y = time_per_move, color = time_class)) +
        geom_smooth() +
        ylab("Time Spent Per Move (Seconds)") +
        xlab("Move Number") +
        theme_minimal()
    })
    
    output$TimePlotOutput2 <- renderPlot({
      ggplot(merged_data_complete2, aes(x = move_number, y = Score, size = time_per_move, color = username)) +
        geom_point(alpha = 0.5) +
        ylim(-10, 10) +
        labs(x = "Move Number", y = "Evaluation Score", size = "Time Spent (s)", color = "Player") +
        ggtitle("Bubble Plot of Move Number and Evaluation Score with Time Spent and Players") +
        theme_minimal()
    })
    
    output$TimeTableOutput <- renderDataTable({
      table_data <- merged_data_complete2[, c("move_number", "username", "Score", "time_per_move")]
      datatable(table_data, options = list(pageLength = 10))
    })
    #browser()
  })
  
  observeEvent(input$GetForecastBtn, {
    
    result_df <- result_df()
    merged_data_complete2 <- merged_data_complete2()
    #merged_data_complete2 <- merged_data_complete2[, -ncol(merged_data_complete2)]
    #browser()
    # First, merge result_df and merged_data_complete2
    result_df <- left_join(result_df, merged_data_complete2, by = "game_number")
    # Then slice the result_df to match the number of rows in merged_data_complete2
    result_df <- result_df %>%
      slice(1:nrow(merged_data_complete2))
    username <- input$username
    # Apply all mutations in one pipeline
    #browser()
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
        ),
        game_type_numeric = case_when(
          ChessGameType == "rapid" ~ 1,             # User wins
          ChessGameType == "blitz" ~ 2,             # User loses
          ChessGameType == "bullet" ~ 3,       # Draw
          TRUE ~ NA_real_                  # Handle unexpected cases
        )
      )
    #Score Still ok
    browser()
    # Extract features (X) and target (y)
    result_df2$Score <- as.numeric(result_df2$Score)
    result_df2$year <- as.numeric(result_df2$year)
    result_df2$month <- as.numeric(result_df2$month)
    result_df2$week <- as.numeric(result_df2$week)
    #browser()
    X <- as.matrix(result_df2[, c("opponentElo", "game_type_numeric","game_result_numeric", "time_per_move", "move_number", "Score", "game_number", "year", "week", "month")])
    y_true <- result_df2$current_elo
    # Define the fitness function (e.g., minimize MSE between predicted Elo and actual Elo)
    fitness_function <- function(params, X, y_true) {
      y_pred <- X %*% params
      mse <- mean((y_true - y_pred)^2)
      return(-mse)  # Return the negative because GA maximizes by default
    }
    browser()
    
    ######################UNDER CONSTRUCTION #######################################
    
    # Load necessary libraries
    library(dplyr)
    library(torch)
    
    # Set the seed for reproducibility
    torch_manual_seed(245)
    
    # Define the number of games into the future to predict
    future_steps <- 5
    
    # Create a target variable shifted by the specified number of games within each game_number
    result_df2 <- result_df2 %>%
      group_by(game_number) %>%
      ungroup() %>%
      drop_na()  # Remove rows with NA in the future_elo column due to shifting
    
    # Split the modified result_df into training and test datasets
    data_rows <- floor(0.80 * nrow(result_df2))
    train_indices <- sample(c(1:nrow(result_df2)), data_rows)
    train_data <- result_df2[train_indices, ]
    test_data <- result_df2[-train_indices, ]
    
    # Create the train and test datasets with only the specified columns
    train_data <- result_df2[train_indices, ] %>%
      select(game_result_numeric, game_type_numeric, opponentElo, Score, move_number, time_per_move, month, week, year, current_elo, move_number, game_number) %>%
      drop_na()
    
    test_data <- result_df2[-train_indices, ] %>%
      select(game_result_numeric, game_type_numeric, opponentElo, Score, move_number, time_per_move, month, week, year, current_elo, move_number, game_number) %>%
      drop_na()
    
    # Separate features and target (future_elo) and remove any NA rows
    train_x <- train_data %>% select(-current_elo) %>% drop_na() %>% as.matrix()
    train_y <- train_data %>% pull(current_elo) %>% na.omit() %>% as.matrix()
    
    test_x <- test_data %>% select(-current_elo) %>% drop_na() %>% as.matrix()
    test_y <- test_data %>% pull(current_elo) %>% na.omit() %>% as.matrix()
    
    # Convert data to torch tensors
    train_x <- torch_tensor(train_x, dtype = torch_float())
    train_y <- torch_tensor(train_y, dtype = torch_float())
    test_x <- torch_tensor(test_x, dtype = torch_float())
    test_y <- torch_tensor(test_y, dtype = torch_float())
    
    #browser()
    
    # Define the neural network model
    net <- nn_module(
      initialize = function() {
        self$fc1 <- nn_linear(in_features = ncol(train_x), out_features = 64)
        self$fc2 <- nn_linear(in_features = 64, out_features = 32)
        self$fc3 <- nn_linear(in_features = 32, out_features = 16)
        self$fc4 <- nn_linear(in_features = 16, out_features = 8)
        self$output <- nn_linear(in_features = 8, out_features = 1)
        self$dropout <- nn_dropout(p = 0.2)
      },
      forward = function(x) {
        x %>%
          self$fc1() %>%
          nnf_relu() %>%
          self$fc2() %>%
          nnf_relu() %>%
          self$fc3() %>%
          nnf_relu() %>%
          self$fc4() %>%
          nnf_relu() %>%
          self$output()
      }
    )
    
    model <- net()
    
    # Define the optimizer and loss function
    optimizer <- optim_adam(model$parameters, lr = 0.01)
    loss_fn <- nn_mse_loss()
    
    # Train the model
    num_epochs <- 100
    for (epoch in 1:num_epochs) {
      model$train()
      optimizer$zero_grad()
      output <- model(train_x)
      loss <- loss_fn(output, train_y)
      loss$backward()
      optimizer$step()
      
      if (epoch %% 10 == 0) {
        cat("Epoch:", epoch, "Loss:", loss$item(), "\n")
      }
    }
    
    # Evaluate the model with condition on game number
    model$eval()
    last_game_number <- NULL  # Initialize a variable to store the last game number
    
    # Extract game numbers from test_data
    test_game_numbers <- test_data$game_number
    
    with_no_grad({
      predictions <- vector("list", nrow(test_x))  # Preallocate list for predictions
      
      for (i in 1:nrow(test_x)) {
        current_game_number <- test_game_numbers[i]  # Get the current game number from test_data
        
        # Check if the current game number is different from the last one
        if (!identical(current_game_number, last_game_number)) {
          prediction <- model(test_x[i, , drop = FALSE])  # Make prediction
          predictions[[i]] <- as_array(prediction)  # Store prediction
          
          # Update last game number
          last_game_number <- current_game_number
        } else {
          # Copy the previous prediction for consistency within the game
          predictions[[i]] <- predictions[[i - 1]]
        }
      }
      
      # Convert list of predictions to a tensor for evaluation
      predictions <- torch_tensor(unlist(predictions), dtype = torch_float())
      
      # Calculate Mean Absolute Error (MAE)
      mae <- mean(abs(predictions - test_y))
      cat("Test MAE:", mae$item(), "\n")
      
      # Calculate Root Mean Squared Error (RMSE)
      rmse <- sqrt(mean((predictions - test_y)^2))
      cat("Test RMSE:", rmse$item(), "\n")
      
      # Calculate R²
      ss_res <- sum((predictions - test_y)^2)
      ss_tot <- sum((test_y - mean(test_y))^2)
      r_squared <- 1 - (ss_res / ss_tot)
      cat("Test R²:", r_squared$item(), "\n")
    })
    
    # Convert the tensors to R vectors
    predictions_vector <- as_array(predictions)
    test_y_vector <- as_array(test_y)
    game_numbers_vector <- test_data$game_number  # Extract game numbers for the test set
    
    # Create a results data frame including game numbers
    results <- data.frame(Game_Number = game_numbers_vector, 
                          Predicted = predictions_vector, 
                          Actual = test_y_vector)
    
    # Remove duplicates, keeping only the first occurrence per game_number
    unique_results <- results %>%
      group_by(Game_Number) %>%
      summarize(Predicted = first(Predicted), 
                Actual = first(Actual), 
                .groups = 'drop')  # Drop grouping after summarization
    
    # Print the unique results
    print(unique_results)
    
    ########################################################
    
    
    
    
    
    
    
    
    # Run the genetic algorithm to find the optimal weights
    ga_model <- ga(
      type = "real-valued",
      fitness = function(params) fitness_function(params, X, y_true),
      lower = rep(-1, ncol(X)),
      upper = rep(1, ncol(X)),
      popSize = 5000,
      maxiter = 100000,
      run = 10,
      pmutation = 0.25
    )
    
    # Initialize the current state (last known values)
    current_state <- as.numeric(result_df2[nrow(result_df2), c("opponentElo", "game_result_numeric","game_type_numeric", "time_per_move", "move_number", "Score", "game_number", "year", "week", "month")])
    
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
    
  
})
  
}


#python3.8 -m pip install tensorflow


  