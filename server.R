# Define the server
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)


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
      pgn_values <- as.data.frame(t(pgn_values), stringsAsFactors = FALSE)
      pgn_values$ChessGameType <- combined_df$games$time_class[i]
      return(pgn_values)
    })
    
    result_df <- bind_rows(result_list) %>% na.omit()
    
    if (desired_rows < nrow(result_df)) {
      result_df <- tail(result_df, desired_rows)
    }
    
    result_df$V3 <- clean_dates(result_df$V3)
    result_df$V3 <- as.Date(result_df$V3)
    result_df$V7 <- sapply(result_df$V7, clean_results)
    result_df[, c(14, 15)] <- sapply(result_df[, c(14, 15)], clean_elo)
    result_df$V16 <- sapply(result_df$V16, clean_elo)
    result_df[, c(18, 20)] <- lapply(result_df[, c(18, 20)], clean_time)
    result_df[, c(5, 6)] <- sapply(result_df[, c(5, 6)], clean_usernames)
    
    result_df$elo_change <- NA
    result_df$total_elo_change <- NA
    
    for (i in 1:nrow(result_df)) {
      if (result_df[i, "V5"] == chess_username) {
        current_elo <- result_df[i, "V14"]
        opponent_elo <- result_df[i, "V15"]
      } else if (result_df[i, "V6"] == chess_username) {
        current_elo <- result_df[i, "V15"]
        opponent_elo <- result_df[i, "V14"]
      } else {
        next
      }
      
      if (i == 1) {
        result_df[i, "elo_change"] <- 0
      } else {
        previous_elo <- ifelse(result_df[i - 1, "V5"] == chess_username, 
                               result_df[i - 1, "V14"], 
                               result_df[i - 1, "V15"])
        elo_change <- current_elo - previous_elo
        result_df[i, "elo_change"] <- elo_change
      }
    }
    
    result_df$total_elo_change <- ave(result_df$elo_change, result_df$V3, FUN = cumsum)
    result_df$elo_change[is.na(result_df$elo_change)] <- 0
    
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
        game_type = result_df$ChessGameType
      ) %>%
      select(-V17, -V21)
    
    result_df$year <- year(result_df$V3)
    result_df$week <- week(result_df$V3)
    result_df$month <- month(result_df$V3)
    
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
    
    updateSelectInput(session, "weekSelect", choices = unique(paste(result_df$year, result_df$week, sep = "-")))
    updateSelectInput(session, "monthSelect", choices = unique(paste(result_df$year, result_df$month, sep = "-")))
    updateSelectInput(session, "YearSelect", choices = unique(paste(result_df$year, sep = "-")))
    

    # Render plots
    output$plotOutput <- renderPlot({
      ggplot(result_df, aes(x = game_number, y = ifelse(input$username == V5, V14, V15), color = game_type)) +
        geom_point() +
        geom_smooth(size = 2) +
        labs(title = "Elo over time", x = "Number of games", y = "Elo") +
        theme_minimal()
    })
    
    output$plotOutput2 <- renderPlot({
      ggplot(result_df, aes(y = total_elo_change, x = week, color = game_type)) +
        geom_smooth() +
        geom_point() +
        labs(title = "Plot of Change in Elo across multiple games in a week", x = "Week", y = "Total Change in Elo")
    })
    
    output$plotOutput3 <- renderPlot({
      ggplot(result_df, aes(x = day_of_week, y = total_elo_change, fill = game_type)) +
        geom_col(position = "dodge") +
        geom_errorbar(aes(ymin = mean(total_elo_change) - sd(total_elo_change) / sqrt(n()), ymax = mean(total_elo_change) + sd(total_elo_change) / sqrt(n())), width = 0.4, position = position_dodge(0.9)) +
        labs(title = "Mean Elo Gain/Loss by Day of the Week", x = "Day of the Week", y = "Mean Elo Change") +
        theme_minimal()
    })
    
    observeEvent(input$YearSelect, {
      selected_years <- input$YearSelect
      
      if (is.null(selected_years) || length(selected_years) == 0) {
        # Handle case where no years are selected
        return(NULL)
      }
      
      if ("All Combined" %in% selected_years) {
        # Display all years combined in one graph
        summary_year <- result_df %>%
          group_by(game_type) %>%
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
          group_by(year, game_type) %>%
          summarize(
            mean_year_elo = mean(total_elo_change, na.rm = TRUE),
            sd_year_elo = sd(total_elo_change, na.rm = TRUE),
            count = n(),
            sem_year_elo = sd(total_elo_change, na.rm = TRUE) / sqrt(n())
          )
      }
      
      output$plotOutput4 <- renderPlotly({
        plot_ly(summary_year, x = ~year, y = ~mean_year_elo, type = "bar", color = ~game_type,
                marker = list(colors = c("#4E79A7", "#E15759", "#76B7B2"))) %>%
          layout(title = "Mean Change in Elo by Year",
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Mean Elo"),
                 showlegend = TRUE)
      })
    })    

    output$plotOutput7 <- renderPlotly({
      plot_ly(summary_month, x = ~month, y = ~mean_month_elo, type = "bar", color = ~game_type,
              marker = list(colors = c("#4E79A7", "#E15759", "#76B7B2"))) %>%
        layout(title = "Mean Change in Elo by Month",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Mean Elo"),
               showlegend = TRUE)
    })
    
    observeEvent(input$weekSelect, {
      selected_week <- strsplit(input$weekSelect, "-")[[1]]
      selected_year <- as.numeric(selected_week[1])
      selected_week <- as.numeric(selected_week[2])
      weekly_data <- result_df %>% filter(year == selected_year, week == selected_week)
      output$plotOutput6 <- renderPlot({
        ggplot(weekly_data, aes(x = V3, y = total_elo_change, color = game_type)) +
          geom_point() +
          geom_line() +
          labs(title = paste("Elo Change for Week", selected_week, "of Year", selected_year), x = "Date", y = "Total Elo Change") +
          theme_minimal()
      })
    })
    
    observeEvent(input$monthSelect, {
      selected_month <- strsplit(input$monthSelect, "-")[[1]]
      selected_year <- as.numeric(selected_month[1])
      selected_month <- as.numeric(selected_month[2])
      monthly_data <- result_df %>% filter(year == selected_year, month == selected_month)
      output$plotOutput5 <- renderPlot({
        ggplot(monthly_data, aes(x = V3, y = total_elo_change, color = game_type)) +
          geom_point() +
          geom_line() +
          labs(title = paste("Elo Change for Month", selected_month, "of Year", selected_year), x = "Date", y = "Total Elo Change") +
          theme_minimal()
      })
    })
  })
}