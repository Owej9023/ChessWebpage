#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


server <- function(input, output, session) {
  observeEvent(input$getDataBtn, {
    chess_username <- input$username
    desired_rows<-input$numberofgames
    api_url <- paste0("https://api.chess.com/pub/player/",chess_username,"/games/archives")
    
    # Make a GET request to the API
    response <- GET(api_url)
    
    
    # Get the content of the response
    response_content <- content(response, "text")
    
    # Parse the JSON content
    archives <- jsonlite::fromJSON(response_content)
    
    # Initialize an empty list to store dataframes
    dataframes <- list()
    #print(archives)
    # Loop through each archive URL and retrieve the data
    archive_data<-list()
    for (url in archives$archives) {
      archive_response <- GET(url)
      archive_content <- content(archive_response, "text", flatten = TRUE)
      archive_data1 <- jsonlite::fromJSON(archive_content)
      archive_data <- c(archive_data,archive_data1)
      
    }
    
    result_list <- list()
    
    combined_df <- bind_rows(archive_data)
    combined_df <- combined_df %>% filter(grepl(input$ChessGameType, time_class, ignore.case = TRUE))
    
    filtered_data <- combined_df %>% filter(rules == "chess")
    filtered_data <- filtered_data %>% filter(rated == "TRUE")
    
    # Assuming filtered_data$pgn is a vector of PGN games
    for (game in filtered_data$pgn) {
      
      # Concatenate the lines of the game
      input_text <- paste(game, collapse = " ")
      
      # Use regex to extract values within square brackets
      matches <- str_extract_all(input_text, "\\[(.*?)\\]")[[1]][1:21]
      
      # Store the result in the list
      result_list <- c(result_list, list(matches))
    }
    
    # Convert the list to a data frame
    result_df <- do.call(rbind, result_list)
    result_df <- as.data.frame(result_df)
    result_df<-na.omit(result_df)
    # Shrink the dataframe to the desired number of rows
    if (desired_rows >= length(result_df$game_number)){
      result_df <- tail(result_df, desired_rows)
    }
    for (i in seq_along(result_df[, 3])) {
      # Extract the date string from the format "[Date "2020.12.08"]"
      date_string <- gsub("\\[Date \"|\"\\]", "", result_df[i, 3])
      
      # Convert the extracted value to a Date object
      #formatted_date <- as.Date(date_string, format = "%Y.%m.%d")
      
      # Replace the original value with the formatted date
      result_df[i, 3] <- date_string
    }
    
    for (i in seq_along(result_df[, 7])) {
      # Extract the result string from the format '[Result "0-1"]' or '[Result "1/2-1/2"]'
      result_string <- as.character(result_df[i, 7])
      
      # Extract the result using a modified regular expression
      result_match <- regmatches(result_string, regexpr("\\d+/\\d+-\\d+/\\d+|\\d+-\\d+", result_string))[[1]]
      
      # Replace the original value with the extracted result
      result_df[i, 7] <- result_match
    }
    
    
    for (i in seq_along(result_df[, 14])) {
      # Extract the Elo rating string from the format '[BlackElo "845"]'
      elo_string <- as.character(result_df[i, 14])
      
      # Extract all numeric values using regular expression
      elo_matches <- regmatches(elo_string, gregexpr("\\d+", elo_string))[[1]]
      
      # Convert the extracted values to numeric
      elo_numeric <- as.numeric(elo_matches)
      
      # Replace the original value with the numeric Elo rating(s)
      result_df[i, 14] <- elo_numeric
    }
    
    for (i in seq_along(result_df[, 15])) {
      # Extract the Elo rating string from the format '[BlackElo "845"]'
      elo_string <- as.character(result_df[i, 15])
      
      # Extract all numeric values using regular expression
      elo_matches <- regmatches(elo_string, gregexpr("\\d+", elo_string))[[1]]
      
      # Convert the extracted values to numeric
      elo_numeric <- as.numeric(elo_matches)
      
      # Replace the original value with the numeric Elo rating(s)
      result_df[i, 15] <- elo_numeric
    }
    
    # Assuming result_df is your data frame
    for (i in seq_along(result_df[, 16])) {
      time <- as.character(result_df[i, 16])
      
      # Extract numeric values using regular expression
      times_numeric <- as.numeric(gsub("\\D", "", time))
      
      # Replace the original value with the numeric time(s)
      result_df[i, 16] <- times_numeric
    }
    
    result_df$V18 <- as.character(result_df$V18)
    
    for (i in seq_along(result_df[, 18])) {
      time_string <- as.character(result_df[i, 18])
      
      # Extract time using regular expression
      time_matches <- regmatches(time_string, regexpr("\\d{2}:\\d{2}:\\d{2}", time_string))[[1]]
      
      # Replace the original value with the numeric time
      result_df[i, 18] <- time_matches
    }
    
    
    # Assuming result_df is your data frame
    for (i in seq_along(result_df[, 20])) {
      time_string <- as.character(result_df[i, 20])
      
      # Extract time using regular expression
      time_matches <- regmatches(time_string, regexpr("\\d{2}:\\d{2}:\\d{2}", time_string))[[1]]
      
      # Replace the original value with the numeric time
      result_df[i, 20] <- time_matches
    }
    
    for (i in seq_along(result_df[, 5])) {
      user_string <- as.character(result_df[i, 5])
      
      # Extract username using regular expression
      extracted_user <- sub("\\[.*?\"(.*?)\"\\]", "\\1", user_string)
      
      # Replace the original value with the extracted username
      result_df[i, 5] <- extracted_user
    }
    
    for (i in seq_along(result_df[, 6])) {
      user_string <- as.character(result_df[i, 6])
      
      # Extract username using regular expression
      extracted_user <- sub("\\[.*?\"(.*?)\"\\]", "\\1", user_string)
      
      # Replace the original value with the extracted username
      result_df[i, 6] <- extracted_user
    }
    
    result_df <- as.data.frame(result_df)
    result_df$V3 <- as.Date(result_df$V3, format = "%Y.%m.%d")
    
    result_df$day_of_week <- weekdays(result_df$V3)
    result_df$day_of_week <- factor(result_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    # Remove columns V17 and V21
    result_df <- result_df %>%
      select(-V17, -V21)
    
    # Mutate the dataframe to create the chess_username column
    result_df <- result_df %>%
      rowwise() %>%
      mutate(
        chess_username = ifelse(chess_username %in% c(V5, V6), as.numeric(ifelse(chess_username %in% V5, V14, V15)), NA_real_)
      )
    
    
    
    # Convert the column to numeric if needed
    result_df$chess_username <- as.numeric(result_df$chess_username)
    
    # Convert 'V14' to numeric (assuming it contains numeric values)
    result_df$V14 <- as.numeric(result_df$V14)
    result_df$V15 <- as.numeric(result_df$V15)
    result_df$V16 <- as.numeric(result_df$V16)
    
    # Convert 'V18' to character
    # Convert 'V18' to hms object
    
    result_df$V18 <- as.character(result_df$V18)
    result_df$V18 <- hms(result_df$V18)
    result_df$V18 <- replace_na(result_df$V18, hms("00:00:00"))
    
    result_df$V20 <- as.character(result_df$V20)
    result_df$V20 <- hms(result_df$V20)
    result_df$V20 <- replace_na(result_df$V20, hms("00:00:00"))
    
    # Extract the hour from the datetime column
    result_df$hour <- hour(result_df$V18)
    result_df$hourend <- hour(result_df$V20)
    result_df$game_number <- 1:nrow(result_df)
    
    
    recent_games_data <- result_df
    
    recent_games_data_games_per_day <- recent_games_data %>%
      group_by(V3) %>%
      mutate(
        total_change_elo = sum(chess_username - lag(chess_username, default = first(chess_username))),
        total_games_per_day = n(),
        group_id = ceiling(row_number() / 10)
      ) %>%
      ungroup()
    
    summary_data <- recent_games_data_games_per_day %>%
      group_by(day_of_week) %>%
      summarize(
        mean_elo_change = mean(total_change_elo),
        sd_elo_change = sd(total_change_elo),
        count = n()
      )
    
    # Calculate standard error of the mean (SEM) for Elo change
    summary_data$sem_elo_change <- summary_data$sd_elo_change / sqrt(summary_data$count)
    
    
    # Create a new column for the week number
    recent_games_data$week_number <- as.numeric(format(as.Date(recent_games_data$V3), "%U"))
    
    # Aggregate by week
    summary_data_week <- recent_games_data %>%
      group_by(week_number) %>%
      summarize(
        mean_eweek_elo = mean(chess_username),  # Change to your actual column name
        sd_eweek_elo = sd(chess_username),  # Change to your actual column name
        count = n()
      )
    
    # Calculate standard error of the mean (SEM) for each week
    summary_data_week$sem_eweek_elo <- summary_data_week$sd_eweek_elo / sqrt(summary_data_week$count)
    
    
    
    # Assuming you have a date column called date_column (replace it with your actual column name)
    # Create a new column for the month
    recent_games_data$month <- factor(format(as.Date(recent_games_data$V3), "%B"), 
                                      levels = month.name, ordered = TRUE)
    
    # Aggregate by month
    summary_data_month <- recent_games_data %>%
      group_by(month) %>%
      summarize(
        mean_emon_elo = mean(chess_username),  # Change to your actual column name
        sd_emon_elo = sd(chess_username),  # Change to your actual column name
        count = n()
      )
    
    # Calculate standard error of the mean (SEM) for each month
    summary_data_month$sem_emon_elo <- summary_data_month$sd_emon_elo / sqrt(summary_data_month$count)


    # Output the result
    #output$textOutput <- renderPrint(result_df)
    output$plotOutput <- renderPlot({ggplot(result_df, aes(x = game_number, y = chess_username)) +
        geom_point() +
        geom_smooth(size = 2) +
        labs(title = "Elo over time",
             x = "Number of games",
             y = "Elo") +
        theme_minimal()})
    #output$plotOutput1 <- renderPlot()
    output$plotOutput2 <- renderPlot({
      # Create a geom_smooth for total change in Elo vs. total games played in a day
      ggplot(recent_games_data_games_per_day, aes(y = total_change_elo, x = total_games_per_day)) +
        geom_smooth(color = "#4E79A7") +
        labs(
          title = "Plot of Change in Elo across multiple games in a day",
          x = "Total # of games played in a day",
          y = "Total Change in Elo"
        )})
    output$plotOutput3 <- renderPlot({    # Create the bar plot with error bars for mean Elo change by day of the week
      ggplot(summary_data, aes(x = day_of_week, y = mean_elo_change)) +
        geom_col(fill = "#4E79A7") +
        geom_errorbar(aes(ymin = mean_elo_change - sem_elo_change, ymax = mean_elo_change + sem_elo_change),
                      width = 0.4, position = position_dodge(0.9)) +
        labs(title = "Mean Elo Gain/Loss by Day of the Week",
             x = "Day of the Week", y = "Mean Elo Change") +
        coord_cartesian(ylim = c(NA, NA)) +  # Adjust the ylim based on your data
        theme_minimal()})
    output$plotOutput4 <- renderPlot({    # Create the bar plot with error bars for mean Elo by week
      ggplot(summary_data_week, aes(x = week_number, y = mean_eweek_elo)) +
        geom_point()+
        geom_smooth()+
        labs(title = "Mean Elo by Week with Standard Dev",
             x = "Week Number", y = "Mean Elo") +
        coord_cartesian(ylim = c(NA, NA)) +
        theme_minimal()})
    output$plotOutput7 <- renderPlotly({
      plot_ly(summary_data_month, x = ~month, y = ~mean_emon_elo, type = "bar", marker = list(color = "#4E79A7")) %>%
        add_trace(
          y = ~mean_emon_elo,
          type = "bar",
          marker = list(color = "#4E79A7"),
          name = "Mean Elo",
          error_y = list(
            type = "data",
            array = ~sd_emon_elo,
            visible = TRUE
          )
        ) %>%
        layout(
          title = "Mean Elo by Month with Standard Dev",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Mean Elo"),
          showlegend = FALSE
        )
    })
    output$plotOutput6 <- renderPlot({})
    output$plotOutput5 <- renderPlot({})
    
  
  })
}
