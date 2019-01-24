# Work with the online data dataframe

# Columns to deal with
names_for_stats <- c("temperature", "specific_conductance", "ph")

# Online data measured just like the offline
online_aggregated_data <- list()

# Line names
line_names <- names(lines_full)

for (i_line in seq(4)) {
  
  # Get online df
  online_data_df <- lines_full[[i_line]]
  
  # Get the offline data timestamps
  offline_timestamps <- offline_data_list[[i_line]]$timestamp
  # Number of observations
  sample_size <- length(offline_timestamps)
  
  # Get previous days
  prev_dates <- lapply(offline_timestamps, function(x) {x - 3.5 * 24 * 60 * 60})
  # Get next days
  next_dates <- lapply(offline_timestamps, function(x) {x + 3.5 * 24 * 60 * 60})
  
  # Average data
  new_data <- list(vector("numeric", sample_size),
                   vector("numeric", sample_size),
                   vector("numeric", sample_size))
  names(new_data) <- names_for_stats
  
  for (j_point in seq(length(offline_timestamps))) {
    # Cut by times
    mask <- (online_data_df$timestamp >= prev_dates[[j_point]]) & (online_data_df$timestamp <= next_dates[[j_point]])
    # Get the subtable
    sub_df <- online_data_df[mask, ]
    
    # Calculate means
    for (col_name in names_for_stats) {
      new_data[[col_name]][j_point] <- na.approx(mean(sub_df[[col_name]], na.rm = TRUE), na.rm = FALSE)
    }
  }
  
  # Make NAs processing
  for (col_name in names_for_stats) {
    # Interpolate
    new_data[[col_name]] <- na.approx(new_data[[col_name]], na.rm = FALSE)
    # Deal with leading and ending
    new_data[[col_name]] <- na.locf(new_data[[col_name]], na.rm = FALSE)
    new_data[[col_name]] <- na.locf(new_data[[col_name]], fromLast = TRUE, na.rm = FALSE)
  }
  
  # Add new column
  new_data[["timestamp"]] <- offline_timestamps
  
  # Perform the online data
  online_aggregated_data[[line_names[[i_line]]]] <- data.frame(new_data)
}
