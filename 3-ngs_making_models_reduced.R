# Make a mask
filtered_bacts <- colnames(variables_dfs[[1]])[-1]
filtered_bacts_inc <- colnames(incoming_vars_df)[-1]

# Calculate the counts
counts_lines <- lapply(variables_dfs, function(x) {data.frame(sum_of_elements = colSums(x[,filtered_bacts]))})
counts_lines[["merged"]] <- do.call("rbind", counts_lines)
counts_lines[["all"]] <- data.frame(sum_of_elements = sapply(filtered_bacts, function(x) { sum(sapply(seq(4), function(y) { sum(variables_dfs[[y]][[x]]) })) }) )

# Bacteria to work with
# Get all
if (TRUE) {
  ngs_vars_names <- filtered_bacts
  ngs_inc_vars_names <- filtered_bacts_inc
} else if (FALSE) {
  # As state variables
  state_vars_count_lim <- 10500
  ngs_vars_names <- filtered_bacts[counts_lines$all$sum_of_elements > state_vars_count_lim]
  # As incoming water
  inc_vars_count_lim <- 10500
  counts_by_dates <- apply(incoming_df[,colnames_bacts], 2, sum)
  ngs_inc_vars_names <- incoming_vars_df[,counts_by_dates > inc_vars_count_lim]
}

# Make interpolation models
ngs_data_models <- list()
if (TRUE) {
  
  for (line_j in seq(1, 4)) {
    
    list_of_models <- list()
    
    for (variable_i in seq(1:length(ngs_vars_names))) {
      
      # Check if there is data
      if (sum(!is.na(variables_dfs[[line_j]][[ngs_vars_names[variable_i]]])) > 1)
      {
        # Make interpolation
        spline_interpolation <- make_spline_interpolation_nonneg(df_data = variables_dfs[[line_j]], 
                                                                 timestamp_col = "timestamp",
                                                                 y_col = ngs_vars_names[variable_i],
                                                                 type = "monoH.FC")
        # Add it to list
        list_of_models[[ngs_vars_names[variable_i]]] <- spline_interpolation
      }
      # Make add to lines list
      ngs_data_models[[paste0("Line ", line_j)]] <- list_of_models
    }
  }
  
  # Initialize the list of modelsfor inc water
  ngs_inc_water_models <- list()
  
  # For incoming water
  for (variable_i in seq(length(ngs_inc_vars_names))) {
    
    # Check if there is data
    if (sum(!is.na(incoming_vars_df[[ngs_inc_vars_names[variable_i]]])) > 1)
    {
      # Make interpolation
      spline_interpolation <- make_spline_interpolation_nonneg(df_data = incoming_vars_df, 
                                                               timestamp_col = "timestamp",
                                                               y_col = ngs_inc_vars_names[variable_i],
                                                               type = "monoH.FC")
      # Add it to list
      ngs_inc_water_models[[ngs_inc_vars_names[variable_i]]] <- spline_interpolation
    }
  }
}
