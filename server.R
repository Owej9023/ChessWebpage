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
library(stringr)
library(purrr)

source("Server_Logic_Chess.R")

server <- function(input, output, session) {

  combined_df <- reactiveVal(NULL)
  
  observeEvent(input$getDataBtn, {
    chess_username <- input$username
    desired_rows <- input$numberofgames
    game_types <- input$ChessGameType
    api_url <- paste0("https://api.chess.com/pub/player/", chess_username, "/games/archives")
    
    # Fetch and parse archive URLs
    archives <- tryCatch({
      jsonlite::fromJSON(content(GET(api_url), "text"))
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
    
    
    #PLAYING WITH REACTIVE VALUES
    combined_df(combined_df)
    
    
    # Process each game to extract PGN values and metadata
    result_list <- lapply(seq_len(nrow(combined_df$games)), function(i) {
      pgn_values <- extract_pgn_values(combined_df$games$pgn[i])
      pgn_values <- as.data.frame(t(pgn_values), stringsAsFactors = FALSE)
      pgn_values$ChessGameType <- combined_df$games$time_class[i]
      return(pgn_values)
    })
    
    result_df <- bind_rows(result_list) %>% na.omit()
    
    if (desired_rows < nrow(result_df)) {
      result_df <- tail(result_df, desired_rows)
    }
    
    result_df$Date <- as.Date(clean_dates(result_df$Date))
    result_df$Result <- sapply(result_df$Result, clean_results)
    result_df$elo_change <- NA
    result_df$total_elo_change <- NA
    
    # Initialize variables to track cumulative Elo change, last Elos by game type, and last game years by game type
    last_elos_by_type <- list()
    cumulative_elo_change_by_year_type <- list()
    last_year_by_type <- list()
    
    # Iterate through each game to calculate elo_change and total_elo_change
    for (i in 1:nrow(result_df)) {
      game_type <- result_df[i, "ChessGameType"]
      white_player <- result_df[i, "White"]
      black_player <- result_df[i, "Black"]
      white_elo <- as.integer(result_df[i, "WhiteElo"])
      black_elo <- as.integer(result_df[i, "BlackElo"])
      game_year <- format(result_df[i, "Date"], "%Y")
      
      # Initialize last Elos, cumulative elo change for the year and type, and last year for the game type if not already done
      if (is.null(last_elos_by_type[[game_type]])) last_elos_by_type[[game_type]] <- NA
      if (is.null(cumulative_elo_change_by_year_type[[game_year]])) cumulative_elo_change_by_year_type[[game_year]] <- list()
      if (is.null(cumulative_elo_change_by_year_type[[game_year]][[game_type]])) cumulative_elo_change_by_year_type[[game_year]][[game_type]] <- 0
      if (is.null(last_year_by_type[[game_type]])) last_year_by_type[[game_type]] <- game_year
      
      # Determine if the current game involves the tracked player
      if (white_player == chess_username) {
        current_elo <- white_elo
        opponent_elo <- black_elo
      } else if (black_player == chess_username) {
        current_elo <- black_elo
        opponent_elo <- white_elo
      } else {
        next # If the tracked player is not involved in the current game, skip to the next iteration
      }
      
      # Calculate elo_change for the current game type
      if (is.na(last_elos_by_type[[game_type]])) {
        result_df[i, "elo_change"] <- 0
      } else {
        previous_elo <- last_elos_by_type[[game_type]]
        result_df[i, "elo_change"] <- current_elo - previous_elo
      }
      
      # Update cumulative elo change for the current year and game type
      cumulative_elo_change_by_year_type[[game_year]][[game_type]] <- cumulative_elo_change_by_year_type[[game_year]][[game_type]] + result_df[i, "elo_change"]
      result_df[i, "total_elo_change"] <- cumulative_elo_change_by_year_type[[game_year]][[game_type]]
      
      # Update the last Elo and last year for the game type
      last_elos_by_type[[game_type]] <- current_elo
      last_year_by_type[[game_type]] <- game_year
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
    
    summary_week <- result_df %>%
      group_by(year, week, ChessGameType) %>%
      summarize(
        mean_week_elo = mean(total_elo_change, na.rm = TRUE),
        sd_week_elo = sd(total_elo_change, na.rm = TRUE),
        count = n(),
        sem_week_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
      )
    
    summary_month <- result_df %>%
      group_by(year, month, ChessGameType) %>%
      summarize(
        mean_month_elo = mean(total_elo_change, na.rm = TRUE),
        sd_month_elo = sd(total_elo_change, na.rm = TRUE),
        count = n(),
        sem_month_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
      )
    
    summary_year <- result_df %>%
      group_by(year, ChessGameType) %>%
      summarize(
        mean_year_elo = mean(total_elo_change, na.rm = TRUE),
        sd_year_elo = sd(total_elo_change, na.rm = TRUE),
        count = n(),
        sem_year_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
      )
    
    # Update the inputs for week, month, and year selection
    updateSelectInput(session, "WeekSelect", choices = unique(paste(result_df$year, result_df$week, sep = "-")))
    updateSelectInput(session, "MonthSelect", choices = unique(paste(result_df$year, result_df$month, sep = "-")))
    updateSelectInput(session, "YearSelect", choices = unique(paste(result_df$year, sep = "-")))
    
    # Render plots
    output$plotOutput <- renderPlot({
      result_df$username_elo <- ifelse(input$username == result_df$White, result_df$WhiteElo, 
                                       ifelse(input$username == result_df$Black, result_df$BlackElo, NA))
      
      ggplot(result_df, aes(x = game_number, y = as.integer(username_elo), color = ChessGameType)) +
        geom_point() +
        geom_smooth(size = 2) +
        labs(title = "Elo over Game Number", x = "Game Number", y = "Elo") +
        theme_minimal()
    })
    
    output$plotOutput2 <- renderPlot({
      ggplot(result_df, aes(x = Date, y = as.integer(total_elo_change), color = ChessGameType)) +
        geom_point() +
        geom_line(size = 2) +
        labs(title = "Total Elo Change over Date", x = "Date", y = "Total Elo Change") +
        theme_minimal()
    })
    
    output$plotOutput3 <- renderPlot({
      ggplot(summary_week, aes(x = week, y = mean_week_elo, group = year, color = ChessGameType)) +
        geom_line(size = 2) +
        labs(title = "Weekly Mean Elo Change", x = "Week", y = "Mean Elo Change") +
        theme_minimal()
    })
    
    output$plotOutput4 <- renderPlotly({
      plot_ly(summary_week, x = ~week, y = ~mean_week_elo, type = "scatter", mode = "lines+markers", color = ~ChessGameType) %>%
        layout(title = "Weekly Elo Change")
    })
    
    output$plotOutput5 <- renderPlotly({
      plot_ly(summary_month, x = ~month, y = ~mean_month_elo, type = "scatter", mode = "lines+markers", color = ~ChessGameType) %>%
        layout(title = "Monthly Elo Change")
    })
    
    output$plotOutput6 <- renderPlotly({
      plot_ly(summary_year, x = ~year, y = ~mean_year_elo, type = "scatter", mode = "lines+markers", color = ~ChessGameType) %>%
        layout(title = "Yearly Elo Change")
    })
  })
  
  #Logic for time
  #currently a little fucky and dosent work because of dataframe mismatching and incorrret row arguments
  observeEvent(input$GetTimeBtn, {
    #Get Reactive Elements
    combined_df<-combined_df()
    
    # Initialize progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Processing games", value = 0)
    on.exit(progress$close())
    
    # Filter data based on the selected time class and number of games
    selected_time_class <- input$timeClass
    num_games_to_plot <- input$numGames
    
    filtered_data <- combined_df %>% 
      filter(games$rules == "chess", 
             games$rated == "TRUE",
             games$time_class %in% selected_time_class) %>% 
      head(num_games_to_plot)
    
    # Initialize lists and counters
    timepermove <- list()
    count <- 0
    total_moves <- sum(sapply(filtered_data$games$pgn, function(pgn) length(str_extract_all(pgn, '\\d+:\\d+(?:\\.\\d+|)]')[[1]])))
    move_counter <- 0
    
    # Loop through each game in filtered_data
    for (game_index in seq_along(filtered_data$games$time_control)) {
      
      # Update progress bar based on the number of moves processed
      progress$set(value = move_counter / total_moves)
      
      value <- filtered_data$games$time_control[game_index]
      
      # Split the string based on the '+'
      split_strings <- strsplit(value, "\\+")[[1]]
      
      # Extract the length of the game for each player in minutes
      Length_of_Game_For_Each_Player <- as.numeric(split_strings[1])
      
      # Convert minutes to HH:MM format
      time_in_hhmm <- sprintf("%02d:%02d", Length_of_Game_For_Each_Player %/% 60, Length_of_Game_For_Each_Player %% 60)
      
      # Set 'set_value' to the formatted time
      set_value <- time_in_hhmm
      
      # Initialize a list to store time per move within the outer loop
      timepermoveinterior <- list()
      
      # Extract all matches of time values in the 'value'
      matches <- str_extract_all(filtered_data$games$pgn[game_index], '\\d+:\\d+(?:\\.\\d+|)]')[[1]]
      set_value <- time_in_hhmm
      set_value1 <- time_in_hhmm
      
      # Get the time_class for the current game
      current_time_class <- filtered_data$games$time_class[game_index]
      
      for (move in seq_along(matches)) {
        if (move %% 2 == 0) {
          m2 <- as.character(matches[move])
          # Convert the string to POSIXct format (minutes and seconds)
          NewMatches <- as.POSIXct(m2, format = "%M:%OS")
          # Calculate time remaining using a custom function 'subtract_time'
          timeRemaining <- subtract_time(set_value, NewMatches)
          # Update 'set_value' for the next iteration
          set_value <- m2
          # Round the time remaining to one decimal place and add it to the interior list
          timeRemaining <- round(timeRemaining, digits = 1)
          timepermoveinterior <- c(timepermoveinterior, timeRemaining)
        } else {
          if (move == 1) {
            m1 <- set_value1
          } else {
            m1 <- as.character(matches[move])
          }
          # Convert the string to POSIXct format (minutes and seconds)
          NewMatches1 <- as.POSIXct(m1, format = "%M:%OS")
          # Calculate time remaining using a custom function 'subtract_time'
          timeRemaining <- subtract_time(set_value1, NewMatches1)
          # Update 'set_value1' for the next iteration
          set_value1 <- m1
          # Round the time remaining to one decimal place and add it to the interior list
          timeRemaining <- round(timeRemaining, digits = 1)
          timepermoveinterior <- c(timepermoveinterior, timeRemaining)
        }
        
        move_counter <- move_counter + 1
        # Update progress bar based on the number of moves processed
        progress$set(value = move_counter / total_moves)
      }
      
      # Add time_per_move and time_class for the current game to the respective lists
      timepermove <- c(timepermove, list(timepermoveinterior))
    }
    
    #progress$close()
    
    # Combine all games into zzztest dataframe
    zzztest <- do.call(rbind, lapply(seq_along(timepermove), function(list_num) {
      inner_list <- timepermove[[list_num]]
      if (length(inner_list) > 0) {
        df <- data.frame(
          time_per_move = unlist(inner_list),
          move_number = seq_along(inner_list),
          game_number = list_num,
          time_class = rep(filtered_data$games$time_class[list_num], length(inner_list))
        )
        return(df)
      } else {
        return(NULL)
      }
    }))
    
    
    #Do the python thing here
    
    #rm(results_df_scorea)
    # Split the PGN data into separate games
    pgn_list <- strsplit(combined_df$games$pgn, "\n\n")
    
    # Initialize lists to store moves and timestamps
    all_moves <- list()
    all_timestamps <- list()
    
    # Loop through each game and extract moves and timestamps
    for (i in seq_along(pgn_list)) {
      game_pgn <- pgn_list[[i]]
      
      # Extract moves and timestamps for the current game
      temp_aa <- extract_moves_and_timestamps(game_pgn)
      temp_aa <- separate_moves_and_timestamps(temp_aa)
      
      # Store the results in the lists
      all_moves[[i]] <- temp_aa$moves
      all_timestamps[[i]] <- temp_aa$timestamps
    }
    
    library(reticulate)
    py_run_string("
import chess
import chess.engine
move_scores = []
def analyze_each_move(moves_str, depth=20, stockfish_path='/path/to/stockfish'):
    # Initialize the Stockfish engine
    with chess.engine.SimpleEngine.popen_uci(stockfish_path) as engine:
        # Set up the board
        board = chess.Board()
        
        # Split the moves string into individual moves
        moves = moves_str.split()
        
        # Store the scores for each move
        
        # Play and analyze each move
        for move in moves:
            # Play the move on the board
            board.push_san(move)
            
            # Analyze the current position
            info = engine.analyse(board, chess.engine.Limit(depth=depth))
            # Check if the position results in a mate
            mate_in = info['score'].relative.mate()
            if mate_in is not None:
              if mate_in == 0:
                score = 10000  # or another appropriate value for immediate mate
              else:
                score = 10000 * (mate_in / abs(mate_in))
            else:
              # Otherwise, use the centipawn score
              score = info['score'].relative.score() / 100
                
            # Adjust the score based on whose turn it is
            if board.turn == chess.BLACK:
                score = -score
                
            # Append the score and the move to the list
            move_scores.append((move, score))
            
        return move_scores
")
    
    
    #########End of the python
    
    
    
    
    
    
    
    
    # Initialize an empty list to store combined data for each game
    combined_data <- list()
    
    # Loop through each game and combine moves and timestamps into a data frame
    for (i in seq_along(all_moves)) {
      # Get moves and timestamps for the current game
      moves <- all_moves[[i]]
      timestamps <- all_timestamps[[i]]
      
      # Check if the number of moves and timestamps are equal
      if (length(moves) == length(timestamps)) {
        # Extract only the timestamp from the format {[%clk 0:09:59.7]}
        cleaned_timestamps <- gsub(".*\\{\\[%clk\\s+([^\\}]+)\\]\\}", "\\1", timestamps)
        
        # Create a data frame for the current game with moves and cleaned timestamps
        game_data <- data.frame(
          Move = moves,
          Timestamp = cleaned_timestamps,
          stringsAsFactors = FALSE
        )
      } else {
        # Handle the case where moves and timestamps are not equal
        warning(paste("Mismatch between moves and timestamps for game", i))
        
        # To avoid error, truncate the longer list to match the shorter one
        min_length <- min(length(moves), length(timestamps))
        
        # Extract only the timestamp from the format {[%clk 0:09:59.7]}
        cleaned_timestamps <- gsub(".*\\{\\[%clk\\s+([^\\}]+)\\]\\}", "\\1", timestamps[1:min_length])
        
        # Create a data frame with the truncated moves and cleaned timestamps
        game_data <- data.frame(
          Move = moves[1:min_length],
          Timestamp = cleaned_timestamps,
          stringsAsFactors = FALSE
        )
      }
      
      # Add the game data frame to the combined list
      combined_data[[i]] <- game_data
    }
    
    NumberOfGames <- input$numGames
    # Initialize an empty list to store combined data for each game
    combined_data <- vector("list", length(all_moves)) # Pre-allocate list for efficiency
    
    # Loop through each game and combine moves and timestamps into a data frame
    for (i in seq_along(all_moves)) {
      moves <- all_moves[[i]]
      timestamps <- all_timestamps[[i]]
      
      # Handle length mismatch and clean timestamps
      min_length <- min(length(moves), length(timestamps))
      cleaned_timestamps <- clean_timestamp(timestamps[1:min_length])
      
      # Create a data frame for the current game with moves and cleaned timestamps
      game_data <- data.frame(
        Move = moves[1:min_length],
        Timestamp = cleaned_timestamps,
        stringsAsFactors = FALSE
      )
      
      # Add the game data frame to the combined list
      combined_data[[i]] <- game_data
    }
    
    combined_data <- lapply(combined_data, function(game_data) {
      game_data$Move <- clean_moves(game_data$Move)
      game_data
    })
    
    # Reset results_df_scorea at the start
    results_df_scorea <- data.frame(Move = character(), Score = numeric(), stringsAsFactors = FALSE)
    
    # Extract moves from the combined data for the first game and analyze
    moves_string <- paste(unlist(combined_data[[NumberOfGames]]$Move), collapse = " ")
    
    # Analyze the combined moves string
    tryCatch({
      aaresults <- py$analyze_each_move(moves_string, depth = 20, stockfish_path = stockfish_path)
      
      # Convert the results to a dataframe and remove duplicates
      game_results_df <- unique(as.data.frame(do.call(rbind, aaresults), stringsAsFactors = FALSE))
      names(game_results_df) <- c("Move", "Score")
      
      # Append results to the main dataframe
      results_df_scorea <- rbind(results_df_scorea, game_results_df)
    }, error = function(e) {
      message(paste("Error analyzing moves:", e$message))
    })
    
    # Extract and merge with zzztest
    zzztest_subset <- zzztest[1:length(results_df_scorea$Move), ]
    merged_data_complete <- cbind(zzztest_subset, results_df_scorea)
    
    # Initialize usernames using a vectorized approach
    usernames <- ifelse(seq_along(merged_data_complete$game_number) %% 2 == 1,
                        combined_df$games$black$username[merged_data_complete$game_number],
                        combined_df$games$white$username[merged_data_complete$game_number])
    
    # Combine the username data frame with the merged data
    merged_data_complete2 <- cbind(data.frame(username = usernames, stringsAsFactors = FALSE), merged_data_complete)
    
    # Remove the 6th column if necessary
    merged_data_complete2 <- merged_data_complete2[, -6]
    merged_data_complete2$Score<-as.numeric(merged_data_complete2$Score)
    
    # Convert the 'Score' column to a numeric vector (if it's not already)
    merged_data_complete2$Score <- unlist(merged_data_complete2$Score)
    
    # Identify even positions
    even_positions <- seq(2, length(merged_data_complete2$Score), by = 2)
    
    # Multiply only the even positions by -1
    merged_data_complete2$Score[even_positions] <- merged_data_complete2$Score[even_positions] * -1
    
    progress$close()
    
    
    output$TimePlotOutput <- renderPlot({
      ggplot(data = zzztest, aes(x = move_number, y = time_per_move, color = time_class)) + 
        geom_smooth()+
        ylab("Time Spent Per Move (Seconds)") +
        xlab("Move Number") +
        theme_minimal()
    })
    
    output$TimePlotOutput2 <- renderPlot({
      ggplot(merged_data_complete2, aes(x = move_number, y = (Score), size = time_per_move, color = username)) +
        geom_point(alpha = 0.5) +
        ylim(-10,10)+
        labs(x = "Move Number", y = "Evaluation Score", size = "Time Spent (s)", color = "Player") +
        ggtitle("Bubble Plot of Move Number and Evaluation Score with Time Spent and Players") +
        theme_minimal()
    })
    
  })

  
  
}

  