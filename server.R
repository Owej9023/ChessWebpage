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
library(shinyWidgets)
library(stringr)
library(purrr)


# Function to extract values from the PGN string
extract_pgn_values <- function(pgn) {
  # Split the PGN string into lines
  pgn_lines <- strsplit(pgn, "\n")[[1]]
  
  # Extract the metadata (everything between square brackets)
  metadata <- pgn_lines[grep("^\\[", pgn_lines)]
  metadata_values <- sapply(metadata, function(line) {
    key_value <- gsub("^\\[|\\]$", "", line)
    key_value_split <- strsplit(key_value, " \"")[[1]]
    value <- gsub("\"$", "", key_value_split[2])
    return(value)
  })
  names(metadata_values) <- sapply(metadata, function(line) {
    key_value <- gsub("^\\[|\\]$", "", line)
    key_value_split <- strsplit(key_value, " \"")[[1]]
    return(key_value_split[1])
  })
  
  # Extract the moves (everything after the blank line)
  moves_index <- which(pgn_lines == "")
  if (length(moves_index) > 0) {
    moves <- pgn_lines[(moves_index[1] + 1):length(pgn_lines)]
    moves <- paste(moves, collapse = " ")
  } else {
    moves <- ""
  }
  
  # Combine the metadata and moves into a single list
  pgn_values <- c(metadata_values, list(moves = moves))
  return(pgn_values)
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
    
    
    result_df <- bind_rows(result_list) %>% na.omit()
    
    if (desired_rows < nrow(result_df)) {
      result_df <- tail(result_df, desired_rows)
    }
    
    result_df$Date <- clean_dates(result_df$Date)
    result_df$Date <- as.Date(result_df$Date)
    result_df$Result <- sapply(result_df$Result, clean_results)
    #result_df[, c(WhiteElo, BlackElo)] <- sapply(result_df[, c(WhiteElo, BlackElo)], clean_elo)
    #result_df$V16 <- sapply(result_df$V16, clean_elo)
    #result_df[, c(18, 20)] <- lapply(result_df[, c(18, 20)], clean_time)
    #result_df[, c(5, 6)] <- sapply(result_df[, c(5, 6)], clean_usernames)
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
      game_year <- format(as.Date(result_df[i, "Date"], format="%Y-%m-%d"), "%Y")
      
      # Initialize last Elos, cumulative elo change for the year and type, and last year for the game type if not already done
      if (is.null(last_elos_by_type[[game_type]])) {
        last_elos_by_type[[game_type]] <- NA
      }
      if (is.null(cumulative_elo_change_by_year_type[[game_year]])) {
        cumulative_elo_change_by_year_type[[game_year]] <- list()
      }
      if (is.null(cumulative_elo_change_by_year_type[[game_year]][[game_type]])) {
        cumulative_elo_change_by_year_type[[game_year]][[game_type]] <- 0
      }
      if (is.null(last_year_by_type[[game_type]])) {
        last_year_by_type[[game_type]] <- game_year
      }
      
      # Determine if the current game involves the tracked player
      if (white_player == chess_username) {
        current_elo <- white_elo
        opponent_elo <- black_elo
      } else if (black_player == chess_username) {
        current_elo <- black_elo
        opponent_elo <- white_elo
      } else {
        # If the tracked player is not involved in the current game, continue to the next iteration
        next
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
    
    
    
    result_df <- result_df %>%
      group_by(Date) %>%
      mutate(total_elo_change = last(total_elo_change)) %>%
      ungroup() %>%
      mutate(
        day_of_week = factor(weekdays(Date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
        chess_username = ifelse(chess_username %in% c(White, Black), as.numeric(ifelse(chess_username %in% White, WhiteElo, BlackElo)), NA_real_),
        hour = ((StartTime)),
        hourend = (EndTime),
        game_number = row_number(),
        game_type = result_df$ChessGameType
      ) %>%
      select(-Termination, -Link)
    
    result_df$year <- year(result_df$Date)
    result_df$week <- week(result_df$Date)
    result_df$month <- month(result_df$Date)
    
    summary_week <- result_df %>%
      group_by(year, week, game_type) %>%
      summarize(
        mean_week_elo = mean(total_elo_change, na.rm = TRUE),
        sd_week_elo = sd(total_elo_change, na.rm = TRUE),
        count = n(),
        sem_week_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
      )
    
    summary_month <- result_df %>%
      group_by(year, month, game_type) %>%
      summarize(
        mean_month_elo = mean(total_elo_change, na.rm = TRUE),
        sd_month_elo = sd(total_elo_change, na.rm = TRUE),
        count = n(),
        sem_month_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
      )
    
    summary_year <- result_df %>%
      group_by(year, game_type) %>%
      summarize(
        mean_year_elo = mean(total_elo_change, na.rm = TRUE),
        sd_year_elo = sd(total_elo_change, na.rm = TRUE),
        count = n(),
        sem_year_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
      )
    
    updateSelectInput(session, "WeekSelect", choices = unique(paste(result_df$year, result_df$week, sep = "-")))
    updateSelectInput(session, "MonthSelect", choices = unique(paste(result_df$year, result_df$month, sep = "-")))
    updateSelectInput(session, "YearSelect", choices = unique(paste(result_df$year, sep = "-")))
    
    # Render plots
    output$plotOutput <- renderPlot({
      # Determine Elo column based on username
      result_df$username_elo <- ifelse(input$username == result_df$White, result_df$WhiteElo, 
                                       ifelse(input$username == result_df$Black, result_df$BlackElo, NA))
      
      ggplot(result_df, aes(x = game_number, y = as.integer(username_elo), color = ChessGameType)) +
        geom_point() +
        geom_smooth(size = 2) +
        labs(title = "Elo over time", x = "Number of games", y = "Elo") +
        theme_minimal()
    })    
    output$plotOutput2 <- renderPlot({
      ggplot(result_df, aes(y = total_elo_change, x = week, color = ChessGameType)) +
        geom_smooth() +
        geom_point() +
        labs(title = "Plot of Change in Elo across multiple games in a week", x = "Week", y = "Total Change in Elo") +
        theme_minimal()
    })
    
    output$plotOutput3 <- renderPlot({
      summary_df <- result_df %>%
        group_by(day_of_week, ChessGameType) %>%
        summarize(
          mean_elo_change = mean(total_elo_change, na.rm = TRUE),
          sem_elo_change = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
        )
      
      ggplot(summary_df, aes(x = day_of_week, y = mean_elo_change, fill = ChessGameType)) +
        geom_col(position = "dodge") +
        geom_errorbar(aes(ymin = mean_elo_change - sem_elo_change, ymax = mean_elo_change + sem_elo_change), 
                      width = 0.4, position = position_dodge(0.9)) +
        labs(title = "Mean Elo Gain/Loss by Day of the Week", x = "Day of the Week", y = "Mean Elo Change") +
        theme_minimal()
    })
    
    #Plot 4
    observeEvent(input$YearSelect, {
      selected_years <- input$YearSelect
      
      if (is.null(selected_years) || length(selected_years) == 0) {
        # Handle case where no years are selected
        return(NULL)
      }
      
      if ("All Combined" %in% selected_years) {
        # Display all years combined in one graph
        summary_year <- result_df %>%
          group_by(ChessGameType) %>%
          summarize(
            mean_year_elo = mean(total_elo_change, na.rm = TRUE),
            sd_year_elo = sd(total_elo_change, na.rm = TRUE),
            count = n(),
            sem_year_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
          ) %>%
          mutate(year = "All Combined")
      } else {
        # Filter data based on selected years
        selected_years <- as.numeric(selected_years)
        yearly_data <- result_df %>% filter(year %in% selected_years)
        
        summary_year <- yearly_data %>%
          group_by(year, ChessGameType) %>%
          summarize(
            mean_year_elo = mean(total_elo_change, na.rm = TRUE),
            sd_year_elo = sd(total_elo_change, na.rm = TRUE),
            count = n(),
            sem_year_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
          )
      }
      
      output$plotOutput4 <- renderPlotly({
        plot_ly(summary_year, x = ~year, y = ~mean_year_elo, type = "bar", color = ~ChessGameType,
                marker = list(colors = c("#4E79A7", "#E15759", "#76B7B2"))) %>%
          layout(title = "Mean Change in Elo by Year",
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Mean Elo"),
                 showlegend = TRUE)
      })
    })
    
    library(lubridate)
    #Plot 5
    observeEvent(input$MonthSelect, {
      selected_months <- input$MonthSelect
      
      if (is.null(selected_months) || length(selected_months) == 0) {
        # Handle case where no months are selected
        return(NULL)
      }
      
      if ("All Combined" %in% selected_months) {
        # Display all months combined in one graph
        summary_month <- result_df %>%
          group_by(ChessGameType) %>%
          summarize(
            mean_month_elo = mean(total_elo_change, na.rm = TRUE),
            sd_month_elo = sd(total_elo_change, na.rm = TRUE),
            count = n(),
            sem_month_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
          ) %>%
          mutate(year_month = "All Combined")
      } else {
        # Extract year and month from selected months
        selected_months <- strsplit(selected_months, "-") %>%
          map(~ list(year = as.numeric(.x[1]), month = as.numeric(.x[2])))
        
        # Filter data based on selected months
        monthly_data <- result_df %>%
          filter(pmap_lgl(list(year, month), function(y, m) {
            any(map_lgl(selected_months, ~ y == .x$year && m == .x$month))
          }))
        
        summary_month <- monthly_data %>%
          group_by(year, month, ChessGameType) %>%
          summarize(
            mean_month_elo = mean(total_elo_change, na.rm = TRUE),
            sd_month_elo = sd(total_elo_change, na.rm = TRUE),
            count = n(),
            sem_month_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop' # Ensure proper ungrouping
          ) %>%
          mutate(year_month = make_date(year, month, 1)) # Create a date object
      }
      
      # Arrange data by year and month numerically
      summary_month <- summary_month %>%
        arrange(year_month)
      
      output$plotOutput5 <- renderPlotly({
        p <- ggplot(summary_month, aes(x = year_month, y = mean_month_elo, color = ChessGameType)) +
          geom_smooth(method = "loess", se = FALSE) +
          geom_point() +  # Add points to visualize the actual data points
          scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
          labs(title = "Mean Change in Elo by Month",
               x = "Year-Month",
               y = "Mean Elo") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        
        ggplotly(p)
      })
    })
    
    #plot 6
    observeEvent(input$WeekSelect, {
      selected_weeks <- input$WeekSelect
      
      if (is.null(selected_weeks) || length(selected_weeks) == 0) {
        # Handle case where no weeks are selected
        return(NULL)
      }
      
      if ("All Combined" %in% selected_weeks) {
        # Display all weeks combined in one graph
        summary_week <- result_df %>%
          group_by(ChessGameType) %>%
          summarize(
            mean_week_elo = mean(total_elo_change, na.rm = TRUE),
            sd_week_elo = sd(total_elo_change, na.rm = TRUE),
            count = n(),
            sem_week_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
          ) %>%
          mutate(year_week = "All Combined")
      } else {
        # Extract year and week from selected weeks
        selected_weeks <- strsplit(selected_weeks, "-") %>%
          map(~ list(year = as.numeric(.x[1]), week = as.numeric(.x[2])))
        
        # Filter data based on selected weeks
        weekly_data <- result_df %>%
          filter(pmap_lgl(list(year, week), function(y, w) {
            any(map_lgl(selected_weeks, ~ y == .x$year && w == .x$week))
          }))
        
        summary_week <- weekly_data %>%
          group_by(year, week, ChessGameType) %>%
          summarize(
            mean_week_elo = mean(total_elo_change, na.rm = TRUE),
            sd_week_elo = sd(total_elo_change, na.rm = TRUE),
            count = n(),
            sem_week_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop' # Ensure proper ungrouping
          ) %>%
          mutate(year_week = as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u")) # Create a date object using ISO week date
      }
      
      # Arrange data by year and week numerically
      summary_week <- summary_week %>%
        arrange(year_week)
      
      # Check for any NA values in year_week and handle them
      if (any(is.na(summary_week$year_week))) {
        summary_week <- summary_week %>%
          filter(!is.na(year_week))
      }
      
      output$plotOutput6 <- renderPlotly({
        p <- ggplot(summary_week, aes(x = year_week, y = mean_week_elo, color = ChessGameType)) +
          geom_smooth(method = "loess", se = FALSE) +
          geom_point() +  # Add points to visualize the actual data points
          scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
          labs(title = "Mean Change in Elo by Week",
               x = "Year-Week",
               y = "Mean Elo") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        
        ggplotly(p)
      })
    })
    
    

      })
  
}

  