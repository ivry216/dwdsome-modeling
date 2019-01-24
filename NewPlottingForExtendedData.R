# Script for making a figures for all the lines online and offline

# Names
ext_list_names <- c("1 online", "2 online", "3 online", "4 online", "1 lab", "2 lab", "3 lab", "4 lab", "input")

# Make a list of all the data
joined_list <- list()
for (i in seq(4)) {
  joined_list[[ext_list_names[i]]] <- online_aggregated_data[[i]]
  joined_list[[ext_list_names[[i + 4]]]] <- offline_data_list[[i]]
}
joined_list[["input"]] <- sis_clean

# Names for axis and file names
names <- c("temperature", "specific_conductance", "ph")
names_axis <- c("Temperature", "Specific Conductance", "pH")
names_file <- c("temper.", "spec. cond.", "ph")

# Colors for the visualization
colors_points <- c("darkred", "goldenrod4", "darkgreen", "darkblue", "red3", "yellow3", "limegreen", "royalblue3", "dimgrey")


# New method for making scatter plots ----------------------------------------------------------------------------
make_scatter_plot_ext <- function(data_lines, line_a, line_b, variable_a, variable_b, names, names_axis) {
  
  # Indetify the column by its name
  col_to_find_a <- names[variable_a]
  col_to_find_b <- names[variable_b]
  name_axis_a <- names_axis[variable_a]
  name_axis_b <- names_axis[variable_b]
  
  # Perform new dataframe
  new_df <- data.frame(data_lines[[line_a]][[col_to_find_a]], data_lines[[line_b]][[col_to_find_b]])
  first_col_name <- paste0(name_axis_a, ", ", line_a)
  second_col_name <- paste0(name_axis_b, ", ", line_b)
  
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
    ggtitle(paste0("Relation between ",  tolower(name_axis_a), ", ", line_a, ", and ", tolower(name_axis_b), ", ", line_b))
  
  return(p)
}

# New method for cross correlation -------------------------------------------------------------------------------
make_ccf_plot <- function(data_lines, line_a, line_b, variable, names, names_axis) {
  
  # Indetify the column by its name
  col_to_find <- names[variable]
  name_axis <- names_axis[variable]
  
  # Perform new dataframe
  first_var <- data_lines[[line_a]][[col_to_find]]
  secon_var <- data_lines[[line_b]][[col_to_find]]
  first_col_name <- paste0(name_axis, ", ", line_a)
  second_col_name <- paste0(name_axis, ", ", line_b)
  
  # Calculate correlation
  cors <- ccf(x = first_var, y = secon_var)
  corelss <- round(cors$acf, 3)
  lags <- cors$lag
  new_df <- data.frame(Lag = lags, ncc = corelss)
  
  p <- ggplot(data = new_df, mapping = aes(x = Lag, y = ncc)) + 
    geom_bar(stat="identity", fill="gold2")+
    geom_text(aes(label=ncc), vjust=-0.3, size=3.5)+
    theme_minimal() + 
    ggtitle(paste("Cross-correlation between", line_a, "and", line_b, "by", tolower(name_axis))) +
    ylab("Normalized Cross-Correlation") + 
    scale_x_continuous(breaks = seq(min(new_df$Lag), max(new_df$Lag), by = 1))
  
  return(p)
}

# Plot all variables scatter plot --------------------------------------------------------------------------------
if (TRUE) {
  for (line_i in seq(1, length(ext_list_names))) {
    for (line_j in seq(line_i, length(ext_list_names))) {
      for (variable_line_i in seq(names)) {
        for (variable_line_j in seq(names)) {
          
          # Check the condition of stopping
          if (line_i == line_j) {
            if (variable_line_i >= variable_line_j) {
              next
            }
          }
          
          # Line names
          line_a_name <- ext_list_names[line_i]
          line_b_name <- ext_list_names[line_j]
          
          png(file = paste0("Corr ", line_a_name, ", ", names_file[variable_line_i],  " and ", line_b_name, ", ", names_file[variable_line_j], ".png"), width = 10, height = 6, units = 'in', res = 300)
          p <- make_scatter_plot_ext(data_lines = joined_list, 
                                    line_a = line_a_name, 
                                    line_b = line_b_name, 
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

# Cross-correlation for all the data -----------------------------------------------------------------------------
if (FALSE) {
  
  # Plot all in a loop
  for (line_i in seq(1, length(ext_list_names) - 1)) {
    for (line_j in seq(line_i + 1, length(ext_list_names))) {
      for (variable_k in seq(names)) {
        
        # Line names
        line_a_name <- ext_list_names[line_i]
        line_b_name <- ext_list_names[line_j]
        
        png(file = paste0(line_a_name, ", ", line_b_name, " ", names_file[variable_k], " cross-corr", ".png"), width = 10, height = 6, units = 'in', res = 300)
        p <- make_ccf_plot(data_lines = joined_list, 
                           line_a = line_a_name, 
                           line_b = line_b_name, 
                           variable = variable_k, 
                           names = names, 
                           names_axis = names_axis)
        print(p)
        dev.off()
      }
    }
  }
}