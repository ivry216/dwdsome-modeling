# Make a mask
filtered_bacts <- colnames_bacts[count_is_zero]

# Calculate the counts
counts_lines <- lapply(variables_dfs, function(x) {data.frame(sum_of_elements = colSums(x[,filtered_bacts]))})
counts_lines[["merged"]] <- do.call("rbind", counts_lines)
counts_lines[["all"]] <- data.frame(sum_of_elements = sapply(filtered_bacts, function(x) { sum(sapply(seq(4), function(y) { sum(variables_dfs[[y]][[x]]) })) }) )

# Colors for histograms
colors_hists <- c("darkred", "goldenrod4", "darkgreen", "darkblue", "gray", "seashell3")

# Limitation
limitation <- 50


# If we need to make histograms
if (TRUE) {
  
  # Make a histograms for all lines
  for (i in seq(4)) {
    
    file_name <- paste0("hist_line_", i, "_lvl_", taxonomy_level, ".png")
    png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
    
    # Filter the data we need
    current_data <- counts_lines[[i]] %>% filter(sum_of_elements < limitation)
    # Make a histogram
    p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
      geom_histogram(binwidth = 1, position="dodge", color = "white", fill = colors_hists[i]) +
      labs(title=paste0("Histogram for Line", i, ", limitation = ", limitation), x = "Number of individuals", y = "Count") +
      scale_x_continuous(breaks = round(seq(min(current_data), max(current_data), by = 1), 1))
    
    print(p)
    dev.off()
  }
  
  # Merged lines
  file_name <- paste0("hist_merge_lines", "_lvl_", taxonomy_level, ".png")
  png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
  
  # Filter the data we need
  current_data <- counts_lines$merged %>% filter(sum_of_elements < limitation)
  # Make a histogram for sum of lines
  p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
    geom_histogram(binwidth = 1, color = "white", fill = colors_hists[5]) +
    labs(title=paste0("Histogram for the merged data, limitation = ", limitation), x = "Number of individuals", y ="Count") +
    scale_x_continuous(breaks = round(seq(min(current_data), max(current_data), by = 1), 1))
  
  print(p)
  dev.off()
  
  
  # Sum of lines
  file_name <- paste0("hist_sum_lines", "_lvl_", taxonomy_level, ".png")
  png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
  
  # Filter the data we need
  current_data <- counts_lines$all %>% filter(sum_of_elements < limitation)
  # Make a histogram for sum of lines
  p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
    geom_histogram(binwidth = 1, color = "white", fill = colors_hists[6]) +
    labs(title=paste0("Histogram for sum of data, limitation = ", limitation), x = "Number of individuals", y ="Count") +
    scale_x_continuous(breaks = round(seq(min(current_data), max(current_data), by = 1), 1))

  print(p)
  dev.off()
}



# If we need to make histograms with no limit
if (TRUE) {
  
  # Make a histograms for all lines
  for (i in seq(4)) {
    
    file_name <- paste0("nolim_hist_line_", i, "_lvl_", taxonomy_level, ".png")
    png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
    
    # Filter the data we need
    current_data <- counts_lines[[i]]
    # Make a histogram
    p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
      geom_histogram(position="dodge", color = "white", fill = colors_hists[i]) +
      labs(title=paste0("Histogram for Line", i), x = "Number of individuals", y = "Count") +
      scale_x_continuous(breaks=c(0,1,5,10,30,100,300,1000,10000), trans = "log1p")
    
    print(p)
    dev.off()
  }
  
  # Merged lines
  file_name <- paste0("nolim_hist_merge_lines", "_lvl_", taxonomy_level, ".png")
  png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
  
  # Filter the data we need
  current_data <- counts_lines$merged
  # Make a histogram for sum of lines
  p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
    geom_histogram(color = "white", fill = colors_hists[5]) +
    labs(title=paste0("Histogram for the merged data limitation"), x = "Number of individuals", y ="Count") +
    scale_x_continuous(breaks=c(0,1,5,10,30,100,300,1000,10000), trans = "log1p")
  
  print(p)
  dev.off()
  
  
  # Sum of lines
  file_name <- paste0("nolim_hist_sum_lines", "_lvl_", taxonomy_level, ".png")
  png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
  
  # Filter the data we need
  current_data <- counts_lines$all
  # Make a histogram for sum of lines
  p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
    geom_histogram(color = "white", fill = colors_hists[6]) +
    labs(title=paste0("Histogram for sum of data"), x = "Number of individuals", y ="Count") +
    scale_x_continuous(breaks=c(0,1,5,10,30,100,300,1000,10000), trans = "log1p")
  
  print(p)
  dev.off()
}

# If we need to make histograms with cumulative sums
if (TRUE) {
  
  # Make a histograms for all lines
  for (i in seq(4)) {
    
    file_name <- paste0("cumulative_line_", i, "_lvl_", taxonomy_level, ".png")
    png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
    
    # Filter the data we need
    current_data <- counts_lines[[i]]
    # Make a histogram
    p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
      geom_histogram(aes(y = length(current_data$sum_of_elements) - cumsum(..count..)), position="dodge", color = "white", fill = colors_hists[i]) +
      labs(title=paste0("Histogram for Line", i), x = "Number of individuals", y = "Count") +
      scale_x_continuous(breaks=c(0,1,5,10,30,100,300,1000,10000), trans = "log1p") + 
      scale_y_continuous(breaks=c(0,10,20,30,40,50,75,100,120))
    
    print(p)
    dev.off()
  }
  
  # Merged lines
  file_name <- paste0("cumulative_hist_merge_lines", "_lvl_", taxonomy_level, ".png")
  png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
  
  # Filter the data we need
  current_data <- counts_lines$merged
  # Make a histogram for sum of lines
  p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
    geom_histogram(aes(y = length(current_data$sum_of_elements) - cumsum(..count..)), color = "white", fill = colors_hists[5]) +
    labs(title=paste0("Histogram for the merged data limitation"), x = "Number of individuals", y ="Count") +
    scale_x_continuous(breaks=c(0,1,5,10,30,100,300,1000,10000), trans = "log1p") +
    scale_y_continuous(breaks=c(0,10,20,30,40,50,75,100,120))
  
  print(p)
  dev.off()
  
  
  # Sum of lines
  file_name <- paste0("cumulative_hist_sum_lines", "_lvl_", taxonomy_level, ".png")
  png(file = file_name, width = 10, height = 6, units = 'in', res = 300)
  
  # Filter the data we need
  current_data <- counts_lines$all
  # Make a histogram for sum of lines
  p <- ggplot(data = current_data, mapping = aes(x = sum_of_elements)) +
    geom_histogram(aes(y = length(current_data$sum_of_elements) - cumsum(..count..)), color = "white", fill = colors_hists[6]) +
    labs(title=paste0("Histogram for sum of data"), x = "Number of individuals", y ="Count") +
    scale_x_continuous(breaks=c(0,1,5,10,30,100,300,1000,10000), trans = "log1p") +
    scale_y_continuous(breaks=c(0,10,20,30,40,50,75,100,120))
  
  print(p)
  dev.off()
}
