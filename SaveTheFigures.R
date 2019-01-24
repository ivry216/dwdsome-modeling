figures_lines_a <- c("Time", 
                     "conductivity", 
                     "Specific Conductance",
                     "Dissolved Oxygen",
                     "Oxidation Reduction Potential",
                     "Ph",
                     "Temperature")

figures_lines_b <- c("Time",
                     "Temperature",
                     "Specific Conductance",
                     "sal_ppt",
                     "depth",
                     "Ph",
                     "ph_mv",
                     "Oxidation Reduction Potential",
                     "Turbidity",
                     "odosat",
                     "Dissolved Oxygen",
                     "battery")

library(plotly)

# --------------------------------------------------------------------------------------------------

# Function for plotting dependency
plot_dependencies <- function(data, x_name, y_name, color = "blue", vline = NULL, x_axis, y_axis, title = NULL) {
  
  p <- ggplot(data = data, mapping = aes_string(x = x_name, y = y_name)) +
    geom_point(color = color, alpha = 0.3)
  
  if (!is.null(vline)) {
    for (i_vline in seq(vline)) {
      p <- p + geom_vline(xintercept=vline[[i_vline]], color = "grey")
    }
  }
  
  p <- p + scale_x_datetime(labels = date_format("%Y-%m-%d"))
  p <- p + labs(x = x_axis, y = y_axis)
  
  return(p)
}

# Function adds NAs to plot
plot_dependencies_with_na <- function(data, x_name, y_name, color = "blue", na_color = "red", vline = NULL, x_axis, y_axis, title = NULL) {
  
  p <- ggplot(data = data, mapping = aes_string(x = x_name, y = y_name)) +
    geom_point(color = color, alpha = 0.3) +
    geom_point(mapping = aes_string(x = x_name, y = "na"), color = na_color)
  
  if (!is.null(vline)) {
    for (i_vline in seq(vline)) {
      p <- p + geom_vline(xintercept=vline[[i_vline]], color = "grey")
    }
  }
  
  p <- p + scale_x_datetime(labels = date_format("%Y-%m-%d"))
  p <- p + labs(x = x_axis, y = y_axis)
  
  return(p)
}

# Plotting dependencies for fimestamp
plot_dependencies_ts <- function(data, y_name, color = "blue", na_value = NULL, na_color = "red", vline = NULL, x_axis, y_axis, title = NULL) {
  
  # If we're plotting only the data without NAs
  if (is.null(na_value)) {
    return(plot_dependencies(data, "timestamp", y_name, color, vline, x_axis, y_axis, title))
  }
  
  #
  data[["na"]] <- NA 
  data[["na"]][is.na(data[[y_name]])] <- na_value 
  
  p <- plot_dependencies_with_na(data, "timestamp", y_name, color, na_color, vline, x_axis, y_axis, title)
  
  return(p)
}

# --------------------------------------------------------------------------------------------------

# Save all to file
if (TRUE) {
  
  draw_simple <- FALSE
  draw_sample_na <- FALSE
  draw_na_0 <- FALSE
  draw_na_mean <- FALSE
  draw_vlines_start <- FALSE
  
  data_for_figures <- data_to_postporcess
  data_all_for_figures <- data_to_postporcess
  
  # Fix parent directore
  this_wd <- getwd()
  
  iterator <- 0
  
  # Set the name of X axis
  x_axis = figures_lines_a[[1]]
  
  # For all lines...
  for (i_line in seq(data_for_figures)) {
    iterator <- iterator + 1
    
    line_dir_name <- i_line
    
    # Create directory
    dir.create(file.path(this_wd, line_dir_name))
    setwd(file.path(this_wd, line_dir_name))
    
    # For all columns
    column_names <- colnames(data_for_figures[[i_line]])
    
    # If weare drawing vertical lines
    vertical_lines <- NULL
    if (draw_vlines_start) {
      vertical_lines <- list()
      for (j_df in seq(clean_comments[[i_line]])) {
        vertical_lines[[j_df]] <- clean_comments[[i_line]][[j_df]][["start"]]
      }
    }
    
    j_iterator <- 1
    for (j_column in seq(column_names)[-1]) {
      j_iterator <- j_iterator + 1
      
      # Find the name for the Y axis
      if (iterator < 3) {
        y_axis = figures_lines_a[j_iterator]
      } else {
        y_axis = figures_lines_b[j_iterator]
      }
      
      
      if (draw_simple) {
        png(file = paste0("Line ", i_line, " ", column_names[j_column], "_noNA.png"), width = 10, height = 6, units = 'in', res = 300)
        p <- plot_dependencies(data = data_for_figures[[i_line]], x_name = "timestamp", y = column_names[j_column], vline = vertical_lines, x_axis = x_axis, y_axis = y_axis)
        print(p)
        dev.off()
      }
      
      if (draw_sample_na) {
        png(file = paste0("Line ", i_line, " ", column_names[j_column], "_innerNA.png"), width = 10, height = 6, units = 'in', res = 300)
        p_na <- plot_dependencies_ts(data = data_for_figures[[i_line]], y_name = column_names[j_column], na_value = 0, x_axis = x_axis, y_axis = y_axis)
        print(p_na)
        dev.off()
      }

      if (draw_na_0) {
        png(file = paste0("Line ", i_line, " ", column_names[j_column], "_allNA.png"), width = 10, height = 6, units = 'in', res = 300)
        p_all_na <- plot_dependencies_ts(data = data_all_for_figures[[i_line]], column_names[j_column], na_value = 0, x_axis = x_axis, y_axis = y_axis)
        print(p_all_na)
        dev.off()
      }
      
      if (draw_na_mean) {
        png(file = paste0("Line ", i_line, " ", column_names[j_column], "_allNA_mean.png"), width = 10, height = 6, units = 'in', res = 300)
        p_all_na <- plot_dependencies_ts(data = data_all_for_figures[[i_line]], column_names[j_column], na_value = mean(data_all_for_figures[[i_line]][[column_names[j_column]]], na.rm = TRUE), x_axis = x_axis, y_axis = y_axis)
        print(p_all_na)
        dev.off()
      }
    }
    
    setwd(file.path(this_wd))
  }
  
}

# --------------------------------------------------------------------------------------------------

plot_multiple <- TRUE
plot_multiple_ly <- FALSE

names_for_multiple_plotting <- c("temperature", "specific_conductance", "dissolved_oxygen", "orp", "ph")
axis_names_for_multiple <- c("Temperature", "Specific Conductance", "Dissolved Oxygen", "Oxidation Reduction Potential", "Ph")


plot_many_dependencies <- function(list_of_data, x_name, y_name, list_of_colors, x_axis, y_axis) {
  p <- ggplot(NULL, mapping = aes_string(x = x_name, y = y_name)) +
    geom_point(data = list_of_data[[1]], color = list_of_colors[[1]], alpha = 0.5, size = 1) +
    geom_point(data = list_of_data[[2]], color = list_of_colors[[2]], alpha = 0.5, size = 1) +
    geom_point(data = list_of_data[[3]], color = list_of_colors[[3]], alpha = 0.5, size = 1) +
    geom_point(data = list_of_data[[4]], color = list_of_colors[[4]], alpha = 0.5, size = 1) +
    labs(x = x_axis, y = y_axis)

  return(p)
}

plot_many_dependencies_ly <- function(list_of_data, x_name, y_name, list_of_colors, list_of_names, x_axis, y_axis) {
  p <- plot_ly()
  
  iter <- 0
  for (df in list_of_data) {
    iter <- iter + 1
    p <- add_markers(p, x = df[[x_name]], y = df[[y_name]], opacity = 0.8, name = list_of_names[[iter]])
  }
  
  p <- p %>%
    layout(xaxis = list(name = x_axis), yaxis = list(name = y_axis))
  
  return(p)
}

if (plot_multiple) {
  
  iterator <- 0
  for (name in names_for_multiple_plotting) {
    iterator <- iterator + 1
    
    png(file = paste0(name, "all_noNA.png"), width = 20, height = 8, units = 'in', res = 300)
    p <- plot_many_dependencies(list_of_data = data_for_figures, x_name = "timestamp", y_name = name, list_of_colors = c("red3", "yellow3", "limegreen", "royalblue3"), x_axis = "Time", y_axis = axis_names_for_multiple[iterator])
    print(p)
    dev.off()
  }
  
}

if (plot_multiple_ly) {
  
  iterator <- 0
  for (name in names_for_multiple_plotting) {
    iterator <- iterator + 1
    
    p <- plot_many_dependencies_ly(list_of_data = data_for_figures, x_name = "timestamp", y_name = name, list_of_colors = c("red3", "yellow3", "limegreen", "royalblue3"), list_of_names = line_names, x_axis = "Time", y_axis = axis_names_for_multiple[iterator])
    htmlwidgets::saveWidget(p, paste0(name, "allNoNA_plt",".html"))
  }
  
}

# --------------------------------------------------------------------------------------------------