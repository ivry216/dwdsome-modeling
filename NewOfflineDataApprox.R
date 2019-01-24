require(ggplot2)

# Colors for data
colors_points <- c("darkred", "goldenrod4", "darkgreen", "darkblue")
colors_approx <- c("red4", "yellow4", "green4", "blue4")

# Names for Y axis
ext_y_axis_names <- c("Temperature",
                           "Ph",
                           "Specific Conductance",
                           "Cu",
                           "Fe",
                           "Chlorine total",
                           "Chlorine free",
                           "Chloramine",
                           "R2A",
                           "Turbidity",
                           "DAPI",
                           "ATP")

make_spline_interpolation_nonneg <- function(df_data, timestamp_col, y_col, normalization = 100000, type = "fmm") {
  
  # All times to numeric
  times <- as.numeric(df_data[[timestamp_col]])
  # Find the starting time
  min_time <- min(times)
  # Normalize times
  times <- times - min_time
  times <- times / normalization
  
  # Make a spline interpolation
  spline_function <- splinefun(x = times, y = df_data[[y_col]], method = type)
  
  # Perform a spline interpolation with shifting and normalization
  res_function <- function(x) {
    var_x <- (x - min_time) / normalization
    var_y <- spline_function(var_x)
    # Make it nonnegatie
    var_y[var_y < 0] <- 0
    return(var_y)
  }
  
  return(res_function)
}

make_spline_interpolation_nonneg_dis <- function(df_data, timestamp_col, y_col, normalization = 100000, type = "fmm") {
  
  # All times to numeric
  times <- as.numeric(df_data[[timestamp_col]])
  # Find the starting time
  min_time <- min(times)
  # Fid the max time
  max_time <- max(times)
  # Find max value
  max_value <- df_data[[y_col]][length(df_data[[y_col]])]
  # Normalize times
  times <- times - min_time
  times <- times / normalization
  
  # Make a spline interpolation
  spline_function <- splinefun(x = times, y = df_data[[y_col]], method = type)
  
  # Perform a spline interpolation with shifting and normalization
  res_function <- function(x) {
    var_x <- (x - min_time) / normalization
    # Get approximation
    var_y <- spline_function(var_x)
    # Make it nonnegatie
    var_y[var_y < 0] <- 0
    # Make 0 all that had negative x
    var_y[x < min_time] <- 0
    # If the time is more than the last disinfection rate change, it keeps the last value
    var_y[x > max_time] <- max_value
    return(var_y)
  }
  
  return(res_function)
}

# Make interpolation models
offline_data_models <- list()
if (TRUE) {
  
  for (line_j in seq(1, 4)) {
    
    list_of_models <- list()
    
    for (variable_i in seq(1:length(ext_offline_variables))) {
      
      # Check if there is data
      if (sum(!is.na(ext_offline_list[[line_j]][[ext_offline_variables[variable_i]]])) > 1)
      {
        # Make interpolation
        spline_interpolation <- make_spline_interpolation_nonneg(df_data = ext_offline_list[[line_j]], 
                                                                 timestamp_col = "timestamp",
                                                                 y_col = ext_offline_variables[variable_i],
                                                                 type = "monoH.FC")
        # Add it to list
        list_of_models[[ext_offline_variables[variable_i]]] <- spline_interpolation
      }
      # Make add to lines list
      offline_data_models[[paste0("Line ", line_j)]] <- list_of_models
    }
  }
}


# Draw all the lines for the same variable on the same plot
if (FALSE) {
  
  for (variable_i in seq(1:length(ext_offline_variables))) {
    
    png(file = paste0("Approxs ", ext_offline_variables[variable_i], ".png"), width = 10, height = 6, units = 'in', res = 300)
    # Plotting
    p <- ggplot(data = NULL, mapping = aes_string(x = "timestamp", y = ext_offline_variables[variable_i]))
    
    for (line_j in seq(1, 4)) {
      
      # Check if there is data
      if (sum(!is.na(ext_offline_list[[line_j]][[ext_offline_variables[variable_i]]])) > 1)
      {
        # Data times
        times <- as.numeric(ext_offline_list[[line_j]][["timestamp"]])
        min_time <- min(times)
        max_time <- max(times)
        data_spline <- data.frame(seq(min_time, max_time, 1000))
        colnames(data_spline)[1] <- "timestamp"
        data_spline <- data_spline %>%
          mutate(!!ext_offline_variables[variable_i] := offline_data_models[[line_j]][[variable_i]](timestamp)) %>%
          mutate(timestamp = as.POSIXct(timestamp, origin = '1970-01-01 UTC', tz="UTC"))
        
        # Names
        point_name <- paste0("Line ", line_j, " obs.")
        curve_name <- paste0("Line ", line_j, " approx.")
        
        # Save to file
        p <- p + 
          geom_point(data = ext_offline_list[[line_j]], mapping = aes(color =!! point_name), size = 3, shape = 15) +
          geom_point(data = data_spline, mapping = aes(color =!! curve_name), size = 1)
      }
      
    }
    
    p <- p + scale_color_manual(name = "Legend", 
                                values=c("red4", "red1", "goldenrod4", "goldenrod1", "green4", "green1", "blue4", "blue1")) + 
      scale_x_datetime(date_labels = "%d/%m") +
      labs(x = "Time", y = ext_y_axis_names[variable_i])
    
    print(p)
    dev.off()
  }
}

# Make interpolation models for inputs
input_data_models <- list()
input_model_names <- colnames(inputs_df)[-1]

if (TRUE) {
  
  input_data_models <- list()
  
  for (variable_i in seq(input_model_names)) {
    
    # Check if there is data
    if (sum(!is.na(inputs_df[[input_model_names[variable_i]]])) > 1)
    {
      # Make interpolation
      spline_interpolation <- make_spline_interpolation_nonneg_dis(df_data = inputs_df, 
                                                                        timestamp_col = "timestamp",
                                                                        y_col = input_model_names[variable_i],
                                                                        type = "monoH.FC")
      # Add it to list
      input_data_models[[input_model_names[variable_i]]] <- spline_interpolation
    }
  }
}

