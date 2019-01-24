# Making a scatterplot of measurements relation

line_a <- 3
line_b <- 4

names <- c("temperature", "specific_conductance", "ph")
names_axis <- c("Temperature", "Specific Conductance", "pH")
names_file <- c("temper.", "spec. cond.", "ph")
colors_points <- c("red3", "yellow3", "limegreen", "royalblue3")

variable <- 1

make_scatter_plot <- function(data_lines, line_a, line_b, variable, names, names_axis) {
  
  # Indetify the column by its name
  col_to_find <- names[variable]
  name_axis <- names_axis[variable]
  
  # Perform new dataframe
  new_df <- data.frame(data_lines[[line_a]][[col_to_find]], data_lines[[line_b]][[col_to_find]])
  first_col_name <- paste0(name_axis, ", Line ", line_a)
  second_col_name <- paste0(name_axis, ", Line ", line_b)

  colnames(new_df) <- c(first_col_name, second_col_name)

  # Calculate correlation
  cor_coef <- round(cor(new_df[[1]], new_df[[2]]), 3)
  
  # Calculate label position
  x_label <- min(new_df[[1]]) + (max(new_df[[1]]) - min(new_df[[1]]))/10
  y_label <- mean(new_df[[2]])
  
  p <- ggplot(data = new_df, mapping = aes_string(x = as.name(first_col_name), y = as.name(second_col_name))) + 
    geom_point(
      color=colors_points[line_j],
      fill=colors_points[line_i],
      shape=21,
      alpha=0.6,
      size=6,
      stroke = 2
    ) + 
    geom_smooth(method = "lm", se = FALSE, color = "deeppink") +
    annotate("text", x = x_label, y = y_label, label= paste("Correlation: ", cor_coef), size = 4) +
    ggtitle(paste("Relation between lines", line_a, "and", line_b, "by", tolower(name_axis)))
  
  return(p)
}


make_scatter_plot_ab <- function(data_lines, line_a, line_b, variable_a, variable_b, names, names_axis) {
  
  # Indetify the column by its name
  col_to_find_a <- names[variable_a]
  col_to_find_b <- names[variable_b]
  name_axis_a <- names_axis[variable_a]
  name_axis_b <- names_axis[variable_b]
  
  # Perform new dataframe
  new_df <- data.frame(data_lines[[line_a]][[col_to_find_a]], data_lines[[line_b]][[col_to_find_b]])
  first_col_name <- paste0(name_axis_a, ", Line ", line_a)
  second_col_name <- paste0(name_axis_b, ", Line ", line_b)
  
  colnames(new_df) <- c(first_col_name, second_col_name)
  
  # Calculate correlation
  cor_coef <- round(cor(new_df[[1]], new_df[[2]]), 3)
  
  # Calculate label position
  x_label <- min(new_df[[1]]) + (max(new_df[[1]]) - min(new_df[[1]]))/10
  y_label <- mean(new_df[[2]])
  
  p <- ggplot(data = new_df, mapping = aes_string(x = as.name(first_col_name), y = as.name(second_col_name))) + 
    geom_point(
      color=colors_points[line_j],
      fill=colors_points[line_i],
      shape=21,
      alpha=0.6,
      size=6,
      stroke = 2
    ) + 
    geom_smooth(method = "lm", se = FALSE, color = "deeppink") +
    annotate("text", x = x_label, y = y_label, label= paste("Correlation: ", cor_coef), size = 4) +
    ggtitle(paste0("Relation between ",  tolower(name_axis_a), ", line ", line_a, ", and ", tolower(name_axis_b), ", line ", line_b))
  
  return(p)
}


# Plot all in a loop
# Plot same variables relation
if (FALSE){
  for (line_i in seq(1, 3)) {
    for (line_j in seq(line_i + 1, 4)) {
      for (variable_k in seq(names)) {
        
        png(file = paste0("Lines ", line_i, ", ", line_j, " ", names_file[variable_k], ".png"), width = 10, height = 6, units = 'in', res = 300)
        p <- make_scatter_plot(data_lines = offline_data_list_plt, 
                               line_a = line_i, 
                               line_b = line_j, 
                               variable = variable_k, 
                               names = names, 
                               names_axis = names_axis)
        print(p)
        dev.off()
      }
    }
  }
}

# Plot all variables relation
if (FALSE) {
  for (line_i in seq(1, 4)) {
    for (line_j in seq(line_i, 4)) {
      for (variable_line_i in seq(names)) {
        for (variable_line_j in seq(names)) {
          
          # Check the condition of stopping
          if (line_i == line_j) {
            if (variable_line_i >= variable_line_j) {
              next
            }
          }
          
          png(file = paste0("Corr. Line ", line_i, ", ", names_file[variable_line_i],  " and line ", line_j, ", ", names_file[variable_line_j], ".png"), width = 10, height = 6, units = 'in', res = 300)
          p <- make_scatter_plot_ab(data_lines = offline_data_list_plt, 
                                 line_a = line_i, 
                                 line_b = line_j, 
                                 variable_a = variable_line_i,
                                 variable_b = variable_line_j,
                                 names = names, 
                                 names_axis = names_axis)
          print(p)
          dev.off()
        }
      }
    }
  }
}


# Online data estimation relation ----------------------------------------------------------------------
# Plot all in a loop
# Plot same variables relation
if (TRUE){
  for (line_i in seq(1, 3)) {
    for (line_j in seq(line_i + 1, 4)) {
      for (variable_k in seq(names)) {
        
        png(file = paste0("Lines ", line_i, ", ", line_j, " ", names_file[variable_k], "_onl.png"), width = 10, height = 6, units = 'in', res = 300)
        p <- make_scatter_plot(data_lines = online_aggegated_data, 
                               line_a = line_i, 
                               line_b = line_j, 
                               variable = variable_k, 
                               names = names, 
                               names_axis = names_axis)
        print(p)
        dev.off()
      }
    }
  }
}

# Plot all variables relation
if (TRUE) {
  for (line_i in seq(1, 4)) {
    for (line_j in seq(line_i, 4)) {
      for (variable_line_i in seq(names)) {
        for (variable_line_j in seq(names)) {
          
          # Check the condition of stopping
          if (line_i == line_j) {
            if (variable_line_i >= variable_line_j) {
              next
            }
          }
          
          png(file = paste0("Corr. Line ", line_i, ", ", names_file[variable_line_i],  " and line ", line_j, ", ", names_file[variable_line_j], "_onl.png"), width = 10, height = 6, units = 'in', res = 300)
          p <- make_scatter_plot_ab(data_lines = online_aggegated_data, 
                                    line_a = line_i, 
                                    line_b = line_j, 
                                    variable_a = variable_line_i,
                                    variable_b = variable_line_j,
                                    names = names, 
                                    names_axis = names_axis)
          print(p)
          dev.off()
        }
      }
    }
  }
}
