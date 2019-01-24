library(lubridate)

# Set the timestampe
initial_timestamps <- ext_offline_list[[1]]$timestamp
# Add additional points?
# Generate the time points like initial + delta + period
if (TRUE) {
  
  # Calculate number of weeks
  number_of_weeks <- length(initial_timestamps) - 2
  # Timestep
  timestep <- 6
  # Timesteps in week
  timesteps_in_week <- floor(24 * 7 / timestep) + 1
  # First week timepoints
  first_week_times <- lapply(seq(timesteps_in_week), function(x) {initial_timestamps[1] + (x - 1) * hours(timestep)})
  
  # Timestamps by week
  # Initialize
  timestamps_by_shift <- list()
  for (i_shift in seq(timesteps_in_week)) {
    # Improve the index
    i_improved <- i_shift - 1
    # These timestamps
    these_timestamps <- list()
    # For all weeks
    for (j_week in seq(number_of_weeks)) {
      # Improve the index
      j_improved <- j_week - 1
      # Make timestamp
      these_timestamps[[j_week]] <- first_week_times[[i_shift]] + j_improved * days(7)
    }
    
    timestamps_by_shift[[i_shift]] <- as.POSIXct(unlist(these_timestamps), origin = "1970-01-01", tz= "UTC")
  }
  
} else if (FALSE) {
  # Generate all the time points with some step
  
  # Calculate number of weeks
  number_of_weeks <- length(initial_timestamps) - 2
  # Timestep
  timestep <- 6
  # Generate the number of steps in a week 
  number_of_steps <- 24*7/timestep
  # First week timepoints
  first_week_times <- lapply(seq(number_of_steps), function(x) {initial_timestamps[1] + x * hours(timestep)})
  generating_timestamps <- list()
  # If we are creating interpolating points
  for (variable in seq(first_week_times)) {
    for (i in seq(0, number_of_weeks)) {
      print(i + 1 + (variable - 1) * number_of_weeks)
      generating_timestamps[[i + 1 + (variable - 1) * number_of_weeks]] <- first_week_times[[variable]] + i*days(7)
    }
  }
  
  generating_timestamps <- unlist(generating_timestamps)
} else {
  # If we use just the initial timestamps
  generating_timestamps <- initial_timestamps
}

generating_timestamps_num <- as.numeric(generating_timestamps)

# Models for offline data: offline_data_models
# Offline models variables: ext_offline_variables

# Models for NGS data: ngs_data_models
# NGS models variables: ngs_vars_names

# Perform a dataframes
generated_dfs <- list()

# Concantenate colnames 
conc_colnames <- c(ext_offline_variables, ngs_vars_names, "timestamp")

for (i in seq(4)) {
  # Make a joined dataframe
  variables <- list()
  # Add offline vars
  for (j in seq(length(ext_offline_variables))) {
    variables[[ext_offline_variables[j]]] <- offline_data_models[[i]][[j]](generating_timestamps_num)
  }
  # Add ngs vars
  for (j in seq(length(ngs_vars_names))) {
    variables[[ngs_vars_names[j]]] <- ngs_data_models[[i]][[j]](generating_timestamps_num)
  }
  # Add timestamps
  variables[["timestamp"]] <- generating_timestamps
  
  # Make a dataframe
  generated_dfs[[paste0('Line ', i)]] <- data.frame(variables)
  # Change the colnames
  colnames(generated_dfs[[paste0('Line ', i)]]) <- conc_colnames
}

# If normalization needed
if (TRUE) {
  # For each line
  for (i_line in seq(4)) {
    # For each row
    for (j_row in seq(nrow(generated_dfs[[i_line]]))) {
      # Find row sum
      row_sum <- sum((generated_dfs[[i_line]] %>% select(one_of(ngs_vars_names)))[j_row, ])
      # Normalize
      generated_dfs[[i_line]][j_row, ngs_vars_names] = generated_dfs[[i_line]][j_row ,ngs_vars_names] / row_sum * 100
    }  
  }
}

# Make the colnames 
if (TRUE) {
  
  new_ngs_names <- unname(sapply(ngs_vars_names, tolower))
  new_ngs_names <- gsub(", ", "_", new_ngs_names)
  
  for (i in seq(4)) {
    colnames(generated_dfs[[i]])[(length(ext_offline_variables)+1):(length(conc_colnames) - 1)] <- new_ngs_names
  }
}

# Need to save XLSX files?
if (FALSE) {
  for (i in seq(4)) {
    write.xlsx(generated_dfs[[i]], file = paste0("tax_lvl_", taxonomy_level, "_line_", i, "_sample.xlsx"))
  }
}