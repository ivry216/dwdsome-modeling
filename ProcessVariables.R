data_to_postporcess <- lines_full

# 1. Functions for cutting all the point-THIS-point outliers
process_neighbour_outliers_cut <- function(data, col_name, threshold) {
  
  # Select the data we need
  this_vector <- data[[col_name]]
  vector_length <- length(this_vector)
  
  for (i in seq(vector_length)) {
    
    # If it is not the first element
    if (i > 1 && i != vector_length) {
      
      # If the current element is not NA
      if (!is.na(this_vector[i])) {
        
        # If the previous is not NA
        if (!is.na(this_vector[i - 1])) {
          previous_distance <- this_vector[i - 1] - this_vector[i]
          
          # If the next is not NA
          if (!is.na(this_vector[i + 1])) {
            next_distance <- this_vector[i + 1] - this_vector[i]
            
            # Calculate the new value of the central element
            if (abs(previous_distance) > threshold && abs(next_distance) > threshold){
              this_vector[i] <- mean(this_vector[i - 1], this_vector[i + 1])
            }
          }
        }
      }
    }
  }
  
  return(this_vector)
}


process_median_outliers_cut <- function(data, col_name, threshold = 3, window_size) {
  
  # Select the data we need
  this_vector <- data[[col_name]]
  vector_length <- length(this_vector)
  
  # Set the half window size
  half_window <- floor((window_size - 1) / 2)
  index_of_touching_end <- vector_length - half_window
  
  # Make a main check
  if (vector_length < window_size) {
    warning("Window size is more than the sample size, the median will be returned")
  }
  
  # Assign filtered vector 
  filtered_vector <- vector("numeric", vector_length)
  
  for (i in seq(vector_length)) {
    
    # if the index is less than the window length
    if (i < half_window) {
      window_start <- 1
    } else {
      window_start <- i - half_window
    }
    
    # if the index is 
    if (i > index_of_touching_end) {
      window_end <- vector_length
    } else {
      window_end <- i + half_window
    }
    
    # Get the subsample
    subsample <- this_vector[window_start:window_end]
    
    # Calculate the median value and variance
    this_median <- median(subsample, na.rm = TRUE)
    this_var <- sd(subsample, na.rm = TRUE)
    
    # If at leaste one value is not NA, -> continue, otherwise -> stop
    if (!is.na(this_median) & !is.na(this_var)) {
      
      if (!is.na(this_vector[i])) {
        if (abs(this_vector[i] - this_median) > this_var * threshold){
          filtered_vector[i] <- this_median
        } else {
          filtered_vector[i] <- this_vector[i]
        }
      } else {
        filtered_vector[i] <- this_median
      }
      
    } else {
      filtered_vector[i] <- NA
    }
  }
  
  return(filtered_vector)
}


process_median_outliers_consequent_cut <- function(data, col_name, threshold = 3, window_size) {
  
  # Select the data we need
  this_vector <- data[[col_name]]
  vector_length <- length(this_vector)
  
  # Set the half window size
  half_window <- floor((window_size - 1) / 2)
  index_of_touching_end <- vector_length - half_window
  
  # Make a main check
  if (vector_length < window_size) {
    warning("Window size is more than the sample size, the median will be returned")
  }
  
  # Assign filtered vector 
  filtered_vector <- vector("numeric", vector_length)
  
  for (i in seq(vector_length)) {
    
    # if the index is less than the window length
    if (i < half_window) {
      window_start <- 1
    } else {
      window_start <- i - half_window
    }
    
    # if the index is 
    if (i > index_of_touching_end) {
      window_end <- vector_length
    } else {
      window_end <- i + half_window
    }
    
    # Get the subsample
    if (i == 1) {
      subsample <- this_vector[window_start:window_end]
    } else if (i == vector_length) {
      subsample <- filtered_vector[window_start:window_end]
    } else {
      subsample[window_start:(i-1)] <- filtered_vector[window_start:(i-1)]
      subsample[i:window_end] <- this_vector[i:window_end]
    }
    
    # Calculate the median value and variance
    this_median <- median(subsample, na.rm = TRUE)
    this_var <- sd(subsample, na.rm = TRUE)
    
    # If at leaste one value is not NA, -> continue, otherwise -> stop
    if (!is.na(this_median) & !is.na(this_var)) {
      
      if (!is.na(this_vector[i])) {
        if (abs(this_vector[i] - this_median) > this_var * threshold){
          filtered_vector[i] <- this_median
        } else {
          filtered_vector[i] <- this_vector[i]
        }
      } else {
        filtered_vector[i] <- this_median
      }
      
    } else {
      filtered_vector[i] <- NA
    }
  }
  
  return(filtered_vector)
}

# ----------------------------------------------------------------------
# Make a ORP corrections for lines 3 and 4
data_to_postporcess$`Linja 3`$orp <- process_neighbour_outliers_cut(data = data_to_postporcess$`Linja 3`, col_name = "orp", threshold = 5)
data_to_postporcess$`Linja 4`$orp <- process_neighbour_outliers_cut(data = data_to_postporcess$`Linja 4`, col_name = "orp", threshold = 5)

# Make dissolved oxygen for lines 1 and 2
data_to_postporcess$`Linja 1`$dissolved_oxygen <- process_neighbour_outliers_cut(data = data_to_postporcess$`Linja 1`, col_name = "dissolved_oxygen", threshold = 1)
data_to_postporcess$`Linja 2`$dissolved_oxygen <- process_neighbour_outliers_cut(data = data_to_postporcess$`Linja 2`, col_name = "dissolved_oxygen", threshold = 1)

# Preprocess the error data of line 4
data_to_postporcess$`Linja 4`$dissolved_oxygen <- process_neighbour_outliers_cut(data = data_to_postporcess$`Linja 4`, col_name = "dissolved_oxygen", threshold = 2.5)
data_to_postporcess$`Linja 4`$specific_conductance <- process_neighbour_outliers_cut(data = data_to_postporcess$`Linja 4`, col_name = "specific_conductance", threshold = 5)


# 2. Smoothing the data
# Specific conductance smoothing for the lines 1 and 2
strategy <- "pracma"

if (strategy == "rolling") {
  data_to_postporcess$`Linja 1`$specific_conductance <- rollmeanr(data_to_postporcess$`Linja 1`$specific_conductance, 30, fill = NA)
  data_to_postporcess$`Linja 2`$specific_conductance <- rollmeanr(data_to_postporcess$`Linja 2`$specific_conductance, 30, fill = NA)
} else if (strategy == "holtwinters") {
  data_to_postporcess$`Linja 1`$specific_conductance <- HoltWinters(data_to_postporcess$`Linja 1`$specific_conductance)
  data_to_postporcess$`Linja 2`$specific_conductance <- HoltWinters(data_to_postporcess$`Linja 2`$specific_conductance)
} else if (strategy == "pracma") {
  data_to_postporcess$`Linja 1`$specific_conductance <- movavg(data_to_postporcess$`Linja 1`$specific_conductance, n = 100, type = "w")
  data_to_postporcess$`Linja 2`$specific_conductance <- movavg(data_to_postporcess$`Linja 2`$specific_conductance, n = 100, type = "w")
}

# Ph smoothing lines 3 and 4
# Smoothing ph
strategy <- "pracma"

if (strategy == "rolling") {
  data_to_postporcess$`Linja 3`$ph <- rollmeanr(data_to_postporcess$`Linja 3`$ph, 30, fill = NA)
  data_to_postporcess$`Linja 4`$ph <- rollmeanr(data_to_postporcess$`Linja 4`$ph, 30, fill = NA)
} else if (strategy == "holtwinters") {
  data_to_postporcess$`Linja 3`$ph <- HoltWinters(data_to_postporcess$`Linja 3`$ph)
  data_to_postporcess$`Linja 4`$ph <- HoltWinters(data_to_postporcess$`Linja 4`$ph)
} else if (strategy == "pracma") {
  data_to_postporcess$`Linja 3`$ph <- movavg(data_to_postporcess$`Linja 3`$ph, n = 100, type = "w")
  data_to_postporcess$`Linja 4`$ph <- movavg(data_to_postporcess$`Linja 4`$ph, n = 100, type = "w")
}

# Dissolved oxygen smoothing
strategy <- "cutter"

if (strategy == "cutter") {
  data_to_postporcess$`Linja 1`$dissolved_oxygen <- process_median_outliers_cut(data = data_to_postporcess$`Linja 1`, col_name = "dissolved_oxygen", threshold = 1, window_size = 150)
  data_to_postporcess$`Linja 2`$dissolved_oxygen <- process_median_outliers_cut(data = data_to_postporcess$`Linja 2`, col_name = "dissolved_oxygen", threshold = 1, window_size = 150)

  data_to_postporcess$`Linja 1`$ph <- process_median_outliers_cut(data = data_to_postporcess$`Linja 1`, col_name = "ph", threshold = 1, window_size = 150)
  data_to_postporcess$`Linja 2`$ph <- process_median_outliers_cut(data = data_to_postporcess$`Linja 2`, col_name = "ph", threshold = 1, window_size = 150)
  
  #data_to_postporcess$`Linja 4`$specific_conductance <- process_median_outliers_cut(data = data_to_postporcess$`Linja 4`, col_name = "specific_conductance", threshold = 1, window_size = 150)
  
}