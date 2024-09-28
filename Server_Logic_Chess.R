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

