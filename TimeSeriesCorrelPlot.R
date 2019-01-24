names <- c("temperature", "specific_conductance", "ph")
names_axis <- c("Temperature", "Specific Conductance", "pH")
names_file <- c("temper.", "spec. cond.", "ph")
colors_points <- c("red3", "yellow3", "limegreen", "royalblue3")
windows <- c(5, 7, 10, 12)

# Rolling correlation plotting

make_rolling_cor_plot <- function(data_lines, line_a, line_b, variable, names, names_axis, window) {
  
  # Indetify the column by its name
  col_to_find <- names[variable]
  name_axis <- names_axis[variable]
  
  # Perform new dataframe
  first_var <- data_lines[[line_a]][[col_to_find]]
  secon_var <- data_lines[[line_b]][[col_to_find]]
  first_col_name <- paste0(name_axis, ", Line ", line_a)
  second_col_name <- paste0(name_axis, ", Line ", line_b)
  
  # Calculate correlation
  cors <- runCor(x = first_var, y = secon_var, n = window)
  cors <- round(cors, 3)
  indexes <- seq(length(cors))
  new_df <- data.frame(Index = indexes, Correlation = cors)
  
  p <- ggplot(data = new_df, mapping = aes(x = Index, y = Correlation)) + 
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=Correlation), vjust=-0.3, size=3.5)+
    theme_minimal() + 
    ggtitle(paste("Rolling correlation between", line_a, "and", line_b, "by", tolower(name_axis))) +
    labs(subtitle = paste("Number of points:", window))
  
  return(p)
}

if (FALSE) {
  # Plot all in a loop
  for (line_i in seq(1, 3)) {
    for (line_j in seq(line_i + 1, 4)) {
      for (variable_k in seq(names)) {
        for (window_s in windows) {
          
          png(file = paste0("Cor. Lines ", line_i, ", ", line_j, " ", names_file[variable_k], ", widnow - ", window_s, ".png"), width = 10, height = 6, units = 'in', res = 300)
          p <- make_rolling_cor_plot(data_lines = offline_data_list, 
                                     line_a = line_i, 
                                     line_b = line_j, 
                                     variable = variable_k, 
                                     names = names, 
                                     names_axis = names_axis, 
                                     window = window_s)
          print(p)
          dev.off()
        }
      }
    }
  }
}


make_ccf_plot <- function(data_lines, line_a, line_b, variable, names, names_axis) {
  
  # Indetify the column by its name
  col_to_find <- names[variable]
  name_axis <- names_axis[variable]
  
  # Perform new dataframe
  first_var <- data_lines[[line_a]][[col_to_find]]
  secon_var <- data_lines[[line_b]][[col_to_find]]
  first_col_name <- paste0(name_axis, ", Line ", line_a)
  second_col_name <- paste0(name_axis, ", Line ", line_b)
  
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

# Cross-correlation for the offline data ---------------------------------------------------------------

if (FALSE) {
  
  # Plot all in a loop
  for (line_i in seq(1, 3)) {
    for (line_j in seq(line_i + 1, 4)) {
      for (variable_k in seq(names)) {
        
        png(file = paste0("Lines ", line_i, ", ", line_j, " ", names_file[variable_k], " cross-corr", ".png"), width = 10, height = 6, units = 'in', res = 300)
        p <- make_ccf_plot(data_lines = offline_data_list_plt, 
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

# Cross-correlation for the online data ----------------------------------------------------------------
if (TRUE) {
  
  # Plot all in a loop
  for (line_i in seq(1, 3)) {
    for (line_j in seq(line_i + 1, 4)) {
      for (variable_k in seq(names)) {
        
        png(file = paste0("Lines ", line_i, ", ", line_j, " ", names_file[variable_k], " cross-corr_onl", ".png"), width = 10, height = 6, units = 'in', res = 300)
        p <- make_ccf_plot(data_lines = online_aggegated_data, 
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