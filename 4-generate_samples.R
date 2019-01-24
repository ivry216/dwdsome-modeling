library(lubridate)

# Set the timestampe
initial_timestamps <- ext_offline_list[[1]]$timestamp

# Generate the time points like initial + delta + period
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


generating_timestamps_num <- list()
for (i in seq(length(timestamps_by_shift))) {
  generating_timestamps_num[[i]] <- as.numeric(timestamps_by_shift[[i]])
}

# Models for offline data: offline_data_models
# Offline models variables: ext_offline_variables

# Models for NGS data: ngs_data_models
# NGS models variables: ngs_vars_names

# Perform a dataframes
generated_dfs_lines <- list()

# Concantenate colnames 
conc_colnames <- c(input_model_names, ngs_inc_vars_names, ext_offline_variables, ngs_vars_names, "timestamp")

for (i in seq(4)) {
  
  # Generated dfs times
  generated_dfs_times <- list()
  
  # 
  for (j in seq(generating_timestamps_num)) {
    # Make a joined dataframe
    variables <- list()
    # Add inputs
    for (k in seq(input_model_names)){
      variables[[paste0("incoming_", input_model_names[k])]] <- input_data_models[[k]](generating_timestamps_num[[j]])
    }
    # Add incoming vars
    for (k in seq(length(ngs_inc_vars_names))) {
      variables[[paste0("incoming_", ngs_inc_vars_names[k])]] <- ngs_inc_water_models[[k]](generating_timestamps_num[[j]])
    }
    # Add offline vars
    for (k in seq(length(ext_offline_variables))) {
      variables[[ext_offline_variables[k]]] <- offline_data_models[[i]][[k]](generating_timestamps_num[[j]])
    }
    # Add ngs vars
    for (k in seq(length(ngs_vars_names))) {
      variables[[ngs_vars_names[k]]] <- ngs_data_models[[i]][[k]](generating_timestamps_num[[j]])
    }
    # Add timestamps
    variables[["timestamp"]] <- timestamps_by_shift[[j]]
    
    # Make a dataframe and add it to list
    generated_dfs_times[[j]] <- data.frame(variables)
    # Change the colnames
    colnames(generated_dfs_times[[j]]) <- conc_colnames
  }

  # Make a dataframe
  generated_dfs_lines[[paste0('Line ', i)]] <- generated_dfs_times
  
}

# If normalization needed
if (FALSE) {
  # For each line
  for (i_line in seq(4)) {
    
    # For each sample
    for (k_sample in seq(generating_timestamps_num)) {
      # For each row
      for (j_row in seq(nrow(generated_dfs_lines[[i_line]][[k_sample]]))) {
        # Find row sum
        row_sum <- sum((generated_dfs_lines[[i_line]][[k_sample]] %>% select(one_of(ngs_vars_names)))[j_row, ])
        # Normalize
        generated_dfs_lines[[i_line]][[k_sample]][j_row, ngs_vars_names] = generated_dfs_lines[[i_line]][[k_sample]][j_row ,ngs_vars_names] / row_sum * 100
      } 
    }

  }
}

# Make the colnames 
if (TRUE) {
  
  new_inp_names <- sapply(input_model_names, function(x) {return(paste0("input_", x))})
  
  #
  new_ngs_names <- unname(sapply(ngs_vars_names, tolower))
  new_ngs_names <- gsub(", ", "_", new_ngs_names)
  
  #
  new_inc_ngs_names <- unname(sapply(ngs_inc_vars_names, tolower))
  new_inc_ngs_names <- unname(sapply(new_inc_ngs_names, function(x) {return(paste0("incoming_", x))}))
  new_inc_ngs_names <- gsub(", ", "_", new_inc_ngs_names)
  
  for (i in seq(4)) {
    for (j in seq(generating_timestamps_num)) {
      colnames(generated_dfs_lines[[i]][[j]])[1:length(new_inp_names)] <- new_inp_names
      colnames(generated_dfs_lines[[i]][[j]])[(length(new_inp_names) + 1):(length(new_inc_ngs_names) + length(new_inp_names))] <- new_inc_ngs_names
      colnames(generated_dfs_lines[[i]][[j]])[(length(new_inp_names) + length(new_inc_ngs_names)+length(ext_offline_variables)+1):(length(conc_colnames) - 1)] <- new_ngs_names  
    }
  }
}

# Need to save XLSX files?
if (FALSE) {
  for (i in seq(4)) {
    for (j in seq(generating_timestamps_num)) {
      write.xlsx(generated_dfs_lines[[i]][[j]], file = paste0("tax_lvl_", taxonomy_level, "_line_", i, "_sample_", j, ".xlsx"))  
    }
  }
}