# Processig the gaps inbetween the measurements

# Initialize the data (only clean df data on the income)
data_raw <- lines

# Datetime column name
time_colname <- "timestamp"

# Get max and min date and convert it to POSIXct again
max_by_col <- as.POSIXct(max(sapply(data_raw, function(x) {return(max(x[[time_colname]]))})), origin = '1970-01-01 UTC', tz="UTC")
min_by_col <- as.POSIXct(min(sapply(data_raw, function(x) {return(min(x[[time_colname]]))})), origin = '1970-01-01 UTC', tz="UTC")

# Number of points 
number_of_points <- as.numeric(difftime(max_by_col, min_by_col, units = "secs")) / (60*5)

# Columns to keep
columns_to_keep <- c("timestamp", 
                     "conductivity", 
                     "specific_conductance",
                     "dissolved_oxygen",
                     "orp",
                     "ph",
                     "temperature")

# Method that is filling the gaps
fill_gaps <- function(data, keep_columns = TRUE, globally = TRUE) {
  
  # Select colnames
  if (keep_columns) {
    new_column_names <- colnames(data)
  } else {
    new_column_names <- columns_to_keep
  }
  
  # Get the number of elements and the filling the timestamp
  if (globally) {
    max_by_col <- as.POSIXct(max(sapply(data_raw, function(x) {return(max(x[[time_colname]]))})), origin = '1970-01-01 UTC', tz="UTC")
    min_by_col <- as.POSIXct(min(sapply(data_raw, function(x) {return(min(x[[time_colname]]))})), origin = '1970-01-01 UTC', tz="UTC")
    number_of_points <- as.numeric(difftime(max_by_col, min_by_col, units = "secs")) / (60*5) + 1
    times <- seq(from = min_by_col, to = max_by_col, length.out = number_of_points)
  } else {
    max_by_col <- as.POSIXct(max(data[[time_colname]]), origin = '1970-01-01 UTC', tz="UTC")
    min_by_col <- as.POSIXct(min(data[[time_colname]]), origin = '1970-01-01 UTC', tz="UTC")
    number_of_points <- as.numeric(difftime(max_by_col, min_by_col, units = "secs")) / (60*5) + 1
    times <- seq(from = min_by_col, to = max_by_col, length.out = number_of_points)
  }
  
  # Create an empty dataframe
  empty_df <- data.frame(matrix(NA, ncol = 1, nrow = number_of_points))
  # Set the colnames
  colnames(empty_df) <- c('timestamp')
  # Fill the timestamp
  empty_df['timestamp'] <- times
  
  resultive_df <- data %>%
    full_join(empty_df)
  
  return(resultive_df)
}

test <- fill_gaps(data = lines[[i_line]], keep_columns = TRUE, globally = TRUE)