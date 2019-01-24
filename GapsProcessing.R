# Processig the gaps inbetween the measurements

# Initialize the data (only clean df data on the income)
data_raw <- clean_lines

# Datetime column name
time_colname <- "timestamp"

# Function that finds closest date which is next
find_closest_next <- function(current_date, dates) {
  next_dates <- dates[current_date <= dates]
  return(min(next_dates))
}

# Function performs df or NULL
perform_NA_df <- function(difference, time_start, time_end, col_names) {

  if (!is.infinite(difference) & (difference > 5)) {
    
    time_sequence <- seq.POSIXt(time_start, time_end, by = "5 min")
    time_sequence <- time_sequence[c(-1, -length(time_sequence))]
    
    start = as.POSIXct(time_sequence[1])
    end = as.POSIXct(time_sequence[length(time_sequence)])
    
    df <- data.frame(matrix(NA, nrow = length(time_sequence), ncol = length(col_names)))
    colnames(df) <- col_names
    df[[time_colname]] <- time_sequence
    
    return(list(df = df, start = start, end = end))
  }
  
  return(NULL)
}

line_gaps <- list()
line_names <- names(data_raw)

# For all lines
for (i_line in seq(data_raw)) {
  
  # Fiand all dfs starts and ends
  df_starts <- sapply(data_raw[[i_line]], function(x) {return(min(x[[time_colname]]))})
  df_ends <- sapply(data_raw[[i_line]], function(x) {return(max(x[[time_colname]]))})
  
  # Find current line start and end
  line_start <- min(df_starts)
  line_end <- max(df_ends)
  
  these_gaps <- list()
  
  for (j_df in seq(data_raw[[i_line]])) {
    
    this_end <- as.POSIXct(df_ends[j_df], origin = '1970-01-01', tz = 'UTC')
    next_start <- as.POSIXct(find_closest_next(this_end, df_starts), origin = '1970-01-01', tz = 'UTC')
    
    difference <- difftime(next_start, this_end, units = "mins")
    print(difference)
    
    result <- perform_NA_df(difference = difference, time_start = this_end, time_end = next_start, col_names = colnames(data_raw[[i_line]][[j_df]]))
    
    if (!is.null(result)) {
      these_gaps[[paste0(result$start, " - ", result$end)]] <- result$df
    }
  }
  
  line_gaps[[line_names[[i_line]]]] <- these_gaps
}