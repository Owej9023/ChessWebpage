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

# Define a function to clean timestamps
clean_timestamp <- function(timestamps) {
  gsub(".*\\[%clk\\s+([^\\}]+)\\].*", "\\1", timestamps)
}

# Clean moves using vectorized operations
clean_moves <- function(moves) {
  moves <- gsub("^\\.\\.\\s+", "", moves)  # Remove ..  from black moves
  moves <- gsub("\\{\\[%clk[^}]*\\]\\}", "", moves)  # Remove clock info
  moves <- gsub("^\\d+\\.\\s*|\\d+\\.\\.\\.\\s*", "", moves)  # Remove move numbers
  trimws(moves)  # Remove leading and trailing whitespace
}

extract_moves_and_timestamps <- function(pgn) {
  lines <- unlist(strsplit(pgn, "\n"))
  moves_line <- lines[length(lines)]
  matches <- regmatches(moves_line, gregexpr("\\d+\\.\\s+[^{}]+\\{\\[%clk\\s+[^\\}]+\\]|\\d+\\.\\.\\.\\s+[^{}]+\\{\\[%clk\\s+[^\\}]+\\]", moves_line))
  moves <- unlist(matches)
  moves_cleaned <- gsub("\\{\\[%clk\\s+([^\\}]+)\\]\\}", " \\1", moves)
  return(moves_cleaned)
}

separate_moves_and_timestamps <- function(data) {
  moves <- c()
  timestamps <- list()
  for (entry in data) {
    if (grepl("^\\d+\\.\\s+", entry)) {
      move <- sub("\\{\\[%clk\\s+([^\\}]+)\\]", "", sub("\\d+\\.\\s+([^{}]+)", "\\1", entry))
    } else if (grepl("^\\d+\\.\\.\\.\\s+", entry)) {
      move <- sub("\\{\\[%clk\\s+([^\\}]+)\\]", "", sub("\\d+\\.\\.\\.\\s+([^{}]+)", "\\1", entry))
    } else {
      next
    }
    timestamp <- sub(".*\\{\\[%clk\\s+([^\\}]+)\\]", "\\1", entry)
    moves <- c(moves, move)
    timestamps <- c(timestamps, timestamp)
  }
  move_string <- paste(moves, collapse = " ")
  timestamp_dict <- as.list(timestamps)
  return(list(moves = moves, timestamps = timestamp_dict))
}

# Create a function that calculates the amount of time spent per move
subtract_time <- function(set_value, variable) {
  set_time <- as.POSIXct(set_value, format = "%M:%OS")
  set_time <- as.numeric(set_time)
  variable_time <- as.POSIXct(variable, format = "%M:%OS")
  variable_time <- as.numeric(variable_time)
  # Subtract the variable time from the set time
  result_time <- set_time - variable_time
  
  # Return the result time
  return(result_time)
}

convert_timestamp_to_seconds <- function(timestamp) {
  parts <- unlist(strsplit(timestamp, "[:.]"))  # Split by colon and dot
  hours <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  tenths <- as.numeric(parts[4]) / 10  # Convert tenths to seconds
  total_seconds <- (hours * 3600) + (minutes * 60) + seconds + tenths
  return(total_seconds)
}