library(lubridate)

# Set the timestampe
initial_timestamps <- ext_offline_list[[1]]$timestamp
# Get the min & max timestamp
min_timestamp <- min(initial_timestamps)
max_timestamp <- max(initial_timestamps)
# Delta in hours
timestamps_per_day <- 5
# Timestamp size
timestamp_size <- 24 / (timestamps_per_day - 1)

# Timestamps for generating samples
timestamps_sample <- seq(min_timestamp, max_timestamp, by = paste0(timestamp_size, " hours"))

# Times to numeric
timestamps_sample_num <- as.numeric(timestamps_sample)

# Models for offline data: offline_data_models
# Offline models variables: ext_offline_variables

# Models for NGS data: ngs_data_models
# NGS models variables: ngs_vars_names

# Perform a dataframes
generated_dfs_lines <- list()

# Concantenate colnames 
conc_colnames <- c(ngs_inc_vars_names, ext_offline_variables, ngs_vars_names, "timestamp")

for (i in seq(4)) {
  
  # Make a joined dataframe
  variables <- list()
  # Add incoming vars
  for (k in seq(length(ngs_inc_vars_names))) {
    variables[[paste0("incoming_", ngs_inc_vars_names[k])]] <- ngs_inc_water_models[[k]](timestamps_sample_num)
  }
  # Add offline vars
  for (k in seq(length(ext_offline_variables))) {
    variables[[paste0("env_", ext_offline_variables[k])]] <- offline_data_models[[i]][[k]](timestamps_sample_num)
  }
  # Add ngs vars
  for (k in seq(length(ngs_vars_names))) {
    variables[[paste0("state_", ngs_vars_names[k])]] <- ngs_data_models[[i]][[k]](timestamps_sample_num)
  }
  # Add timestamps
  variables[["timestamp"]] <- timestamps_sample
  
  # Make a dataframe and add it to list
  line_df <- data.frame(variables)
  # Change the colnames
  colnames(line_df) <- conc_colnames
  
  # Make a dataframe
  generated_dfs_lines[[paste0('Line ', i)]] <- line_df
}

# If normalization needed
if (FALSE) {
  # For each line
  for (i_line in seq(4)) {
    
    # For each row
    for (j_row in seq(nrow(generated_dfs_lines[[i_line]]))) {
      # Find row sum
      row_sum <- sum((generated_dfs_lines[[i_line]] %>% select(one_of(ngs_vars_names)))[j_row, ])
      # Normalize
      generated_dfs_lines[[i_line]][j_row, ngs_vars_names] = generated_dfs_lines[[i_line]][j_row ,ngs_vars_names] / row_sum * 100
    }
    
  }
}

# Make the colnames 
if (TRUE) {
  
  #
  new_ngs_names <- unname(sapply(ngs_vars_names, tolower))
  new_ngs_names <- gsub(", ", "_", new_ngs_names)
  
  #
  new_inc_ngs_names <- unname(sapply(ngs_inc_vars_names, tolower))
  new_inc_ngs_names <- unname(sapply(new_inc_ngs_names, function(x) {return(paste0("incoming_", x))}))
  new_inc_ngs_names <- gsub(", ", "_", new_inc_ngs_names)
  
  for (i in seq(4)) {
    colnames(generated_dfs_lines[[i]])[1:(length(new_inc_ngs_names))] <- new_inc_ngs_names
    colnames(generated_dfs_lines[[i]])[(length(new_inc_ngs_names)+length(ext_offline_variables)+1):(length(conc_colnames) - 1)] <- new_ngs_names  
  }
}

# Need to save XLSX files with whole datasets?
if (FALSE) {
  for (i in seq(4)) {
    write.xlsx(generated_dfs_lines[[i]], file = paste0("plane_tax_lvl_", taxonomy_level, "_line_", i, ".xlsx"))  
  }
}

# Need to save the real output files?
if (FALSE) {
  for (i in seq(4)) {
    write.xlsx(variables_dfs[[i]], file = paste0("real_tax_lvl_", taxonomy_level, "_line_", i, ".xlsx"))  
  }
}