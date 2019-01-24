# Making a spline interpolation

make_spline_interpolation <- function(df_data, timestamp_col, y_col, normalization = 100000, type = "fmm") {
  
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
    
    return(spline_function(var_x))
  }
  
  return(res_function)
}

make_wighted_additive <- function(functions, weights) {
  # Count the number of methods
  n_funcs <- length(functions)
  
  # Perform a result function
  res_function <- function(x) {
    x_var <- as.numeric(x)
    result <- 0
    weights_sum <- 0
    for (i in seq(n_funcs)) {
      result <- result + weights[i] * functions[[i]](x_var)
      weights_sum <- weights_sum + weights[i]
    }
    
    return(result/weights_sum)
  }
  
  return(res_function)
}


# Making plot with splines
names <- c("temperature", "specific_conductance", "ph")
names_axis <- c("Temperature", "Specific Conductance", "pH")
colors_points <- c("red3", "yellow3", "limegreen", "royalblue3")
names_file <- c("temper.", "spec. cond.", "ph")

# Comparing two different splines for offline data
if (FALSE) {
  
  for (line_i in 1:4) {
    for (variable_j in 1:3) {
      
      # Make interpolation
      spline_interpolation <- make_spline_interpolation(df_data = offline_data_list[[line_i]], 
                                                        timestamp_col = "timestamp",
                                                        y_col = names[variable_j],
                                                        type = "monoH.FC")
      natural_spline_interpolation <- make_spline_interpolation(df_data = offline_data_list[[line_i]], 
                                                                timestamp_col = "timestamp",
                                                                y_col = names[variable_j],
                                                                type = "natural")
      
      # Data times
      times <- as.numeric(offline_data_list[[line_i]][["timestamp"]])
      min_time <- min(times)
      max_time <- max(times)
      data_spline <- data.frame(seq(min_time, max_time, 1000))
      colnames(data_spline)[1] <- "timestamp"
      data_spline_fmm <- data_spline %>%
        mutate(!!names[variable_j] := spline_interpolation(timestamp)) %>%
        mutate(timestamp = as.POSIXct(timestamp, origin = '1970-01-01 UTC', tz="UTC"))
      data_spline_natural <- data_spline %>%
        mutate(!!names[variable_j] := natural_spline_interpolation(timestamp)) %>%
        mutate(timestamp = as.POSIXct(timestamp, origin = '1970-01-01 UTC', tz="UTC"))
      
      # Save to file
      png(file = paste0("Spline ", line_i, ", ", names_file[variable_j], ".png"), width = 10, height = 6, units = 'in', res = 300)
      # Plotting
      p <- ggplot(data = online_data_list_plt[[line_i]], mapping = aes_string(x = "timestamp", y = names[variable_j])) +
        geom_point(alpha = 0.5, size = 1, colour = colors_points[line_i]) + 
        geom_point(data = offline_data_list[[line_i]], color = "hotpink", size = 2, shape = 15) +
        scale_x_datetime(date_labels = "%d/%m") +
        geom_point(data = data_spline_fmm, size = 1, colour = "darkmagenta") +
        geom_point(data = data_spline_natural, size = 1, colour = "darkslategray1") +
        labs(x = "Time", y = names_axis[variable_j])
      
      print(p)
      dev.off()
    }
  }
  
}

# Making the splines for the both online and offline data
if (TRUE) {
  
  for (line_i in 1:4) {
    for (variable_j in 1:3) {
      
      # Make interpolation
      offline_spline_interpolation <- make_spline_interpolation(df_data = offline_data_list[[line_i]], 
                                                        timestamp_col = "timestamp",
                                                        y_col = names[variable_j],
                                                        type = "monoH.FC")
      online_spline_interpolation <- make_spline_interpolation(df_data = online_aggegated_data[[line_i]], 
                                                                timestamp_col = "timestamp",
                                                                y_col = names[variable_j],
                                                                type = "monoH.FC")
      weighted_interpolation <- make_wighted_additive(functions = c(offline_spline_interpolation, online_spline_interpolation),
                                                      weights = c(1, 1))
      
      # Data times
      times_offline <- as.numeric(offline_data_list[[line_i]][["timestamp"]])
      min_time_offline <- min(times_offline)
      max_time_offline <- max(times_offline)
      
      times_online <- as.numeric(online_aggegated_data[[line_i]][["timestamp"]])
      min_time_online <- min(times_online)
      max_time_online <- max(times_online)
      
      data_spline_offline <- data.frame(seq(min_time_offline, max_time_offline, 1000))
      colnames(data_spline_offline)[1] <- "timestamp"
      data_spline_online <- data.frame(seq(min_time_online, max_time_online, 1000))
      colnames(data_spline_online)[1] <- "timestamp"
      data_spline_offline <- data_spline_offline %>%
        mutate(!!names[variable_j] := offline_spline_interpolation(timestamp)) %>%
        mutate(timestamp = as.POSIXct(timestamp, origin = '1970-01-01 UTC', tz="UTC"))
      data_spline_online <- data_spline_online %>%
        mutate(!!names[variable_j] := online_spline_interpolation(timestamp)) %>%
        mutate(timestamp = as.POSIXct(timestamp, origin = '1970-01-01 UTC', tz="UTC"))
      data_spline_wighted <- data_spline_online %>%
        mutate(!!names[variable_j] := weighted_interpolation(timestamp)) %>%
        mutate(timestamp = as.POSIXct(timestamp, origin = '1970-01-01 UTC', tz="UTC"))
      
      
      # Save to file
      png(file = paste0("Splines ", line_i, ", ", names_file[variable_j], ".png"), width = 10, height = 6, units = 'in', res = 300)
      # Plotting
      p <- ggplot(data = online_data_list_plt[[line_i]], mapping = aes_string(x = "timestamp", y = names[variable_j])) +
        geom_point(alpha = 0.5, size = 1, colour = colors_points[line_i]) + 
        geom_point(data = offline_data_list[[line_i]], color = "hotpink", size = 2, shape = 15) +
        geom_point(data = online_aggegated_data[[line_i]], color = "darkslategray1", size = 2, shape = 15) +
        scale_x_datetime(date_labels = "%d/%m") +
        geom_point(data = data_spline_offline, size = 1, colour = "darkmagenta") +
        geom_point(data = data_spline_online, size = 1, colour = "darkslategray3") +
        geom_point(data = data_spline_wighted, size = 1, colour = "orange") +
        labs(x = "Time", y = names_axis[variable_j])
      
      print(p)
      dev.off()
    }
  }
  
}