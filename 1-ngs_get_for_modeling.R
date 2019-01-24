# Lib for trimming
library("stringr")
library("dplyr")

# Get the df_map_ext selects for line
select_mapping_for_line <- function(line, mapping) {
  # Get the mask by line
  mask_by_line <- mapping[[paste0("is_line_", line)]]
  # Select rna
  mask_by_rna <- mapping[["is_rna"]]
  # Select water
  mask_by_water <- mapping[["is_water"]]
  
  return(mapping[mask_by_line & mask_by_rna & mask_by_water,])
}

# Columns to select
cols_of_map_to_select <- c("sample_id", "sampling_date")

# Select the mappings for lines
mappings <- list()
for (i in seq(4)) {
  mappings[[paste0("Line ", i)]] <- select_mapping_for_line(i, df_map_ext) %>%
    select(one_of(cols_of_map_to_select))
}

# Get the mapping for incoming water
mapping_incoming <- df_map_ext[df_map_ext$is_incoming == TRUE & df_map_ext$is_rna == TRUE,]

# Get the name of the taxonomy composite
get_sequencing_name <- function(taxonomy_string, index) {
  # Split the taxonomy string by the semicolumn
  values <- strsplit(taxonomy_string, split=';', fixed=TRUE)[[1]]
  return(paste0(str_trim(values[1:index]), collapse = ', '))
}

# Get exact level name
get_exact_level_name <- function(taxonomy_string, index) {
  # Split the taxonomy string by the semicolumn
  values <- strsplit(taxonomy_string, split=';', fixed=TRUE)[[1]]
  value <- str_trim(values[index])
  return(ifelse(value == "NA", NA, value))
}

seq_name_lvl <- vector(mode = "character", length = 7)
exact_name_lvl <- vector(mode = "character", length = 7)

for (i in seq(7)) {
  seq_name_lvl[i] <- paste0("tax_name_lvl_", i)
  exact_name_lvl[i] <- paste0("exact_tax_name_lvl_", i)
  df_table[[seq_name_lvl[i]]] <- sapply(df_table$taxonomy, get_sequencing_name, i)
  df_table[[exact_name_lvl[i]]] <- sapply(df_table$taxonomy, get_exact_level_name, i)
}

# Select the desired level
taxonomy_level <- 4
# The column for that level
seq_level_col <- seq_name_lvl[taxonomy_level]

# Select the taxonomy measurements
variables_dfs <- list()
for (i in seq(4)) {
  
  # Take all the taxonomies
  if (TRUE) {
    # Get the taxonomy grouped information for level
    variables_raw <- df_table %>%
      filter(tax_name_lvl_1 == "Bacteria") %>%
      group_by_(.dots = seq_level_col) %>%
      select(one_of(c(mappings[[i]]$sample_id, seq_level_col))) %>%
      summarise_all(.funs = sum)
  } else {
    # Get the taxonomy grouped information for level
    variables_raw <- df_table %>%
      filter(tax_name_lvl_1 == "Bacteria") %>%
      filter_(.dots = paste0("!is.na(exact_tax_name_lvl_",taxonomy_level,")")) %>%
      group_by_(.dots = seq_level_col) %>%
      select(one_of(c(mappings[[i]]$sample_id, seq_level_col))) %>%
      summarise_all(.funs = sum)
  }
  
  # Select the column names
  selected_colnames <- colnames(variables_raw)[-1]
  
  # Get the data matrix
  matrix_df <- as.matrix(variables_raw[-1])
  # Transpose it
  matrix_transposed <- t(matrix_df)
  
  # Make a new dataframe
  new_df <- as.data.frame(matrix_transposed)
  # Add columns to it
  colnames(new_df) <- variables_raw[[seq_level_col]]
  
  # Get the timestamps
  current_mapping <- mappings[[i]]
  timestamps <- unname(sapply(current_mapping$sample_id, function(x) {current_mapping[current_mapping$sample_id == x,]$sampling_date}))
  # Convert it to POSIXct
  timestamps <- as.POSIXct(timestamps, format="%d.%m.%Y", origin = '1970-01-01', tz = "UTC")
  
  # Add timestamps to df
  new_df['timestamp'] <- timestamps
  
  # Save veriables df
  variables_dfs[[paste0("Line ", i)]] <- new_df %>%
    arrange(timestamp) %>%
    select(timestamp, everything())
}

# ------------------------------------------------------------------------------------------
# Incoming water

# Take all the taxonomies
if (TRUE) {
  # Get the taxonomy grouped information for level
  variables_raw <- df_table %>%
    filter(tax_name_lvl_1 == "Bacteria") %>%
    group_by_(.dots = seq_level_col) %>%
    select(one_of(c(mapping_incoming$sample_id, seq_level_col))) %>%
    summarise_all(.funs = sum)
} else {
  # Get the taxonomy grouped information for level
  variables_raw <- df_table %>%
    filter(tax_name_lvl_1 == "Bacteria") %>%
    filter_(.dots = paste0("!is.na(exact_tax_name_lvl_",taxonomy_level,")")) %>%
    group_by_(.dots = seq_level_col) %>%
    select(one_of(c(mapping_incoming$sample_id, seq_level_col))) %>%
    summarise_all(.funs = sum)
}

# Select the column names
selected_colnames <- colnames(variables_raw)[-1]

# Get the data matrix
matrix_df <- as.matrix(variables_raw[-1])
# Transpose it
matrix_transposed <- t(matrix_df)

# Make a new dataframe
new_df <- as.data.frame(matrix_transposed)
# Add columns to it
colnames(new_df) <- variables_raw[[seq_level_col]]

# Get the timestamps
timestamps <- unname(sapply(mapping_incoming$sample_id, function(x) {mapping_incoming[mapping_incoming$sample_id == x,]$sampling_date}))
# Convert it to POSIXct
timestamps <- as.POSIXct(timestamps, format="%d.%m.%Y", origin = '1970-01-01', tz = "UTC")

# Add timestamps to df
new_df['timestamp'] <- timestamps

# Save veriables df
incoming_df <- new_df %>%
  arrange(timestamp) %>%
  select(timestamp, everything())

# Removing the 0es -------------------------------------------------------------------------
# Get the bacteria names (remove timestamp)
colnames_bacts <- colnames(variables_dfs[[1]])[-1]
print(paste("Number of bacteria before deleting the zeroes:", length(colnames_bacts)))
# Get the sums 
counts <- sapply(colnames_bacts, function(x) { sum(sapply(seq(4), function(y) { sum(variables_dfs[[y]][[x]]) })) })
# Find which counts equals 0
count_is_zero <- counts != 0
# Add timestamp
mask <- c(TRUE, count_is_zero)
# Remove all the empty columns
variables_dfs <- lapply(variables_dfs, function(x) {x[, mask]})
print(paste("Number of bacteria after deleting the zeroes:", length(colnames(variables_dfs[[1]])) - 1))

# Removing the 0es -------------------------------------------------------------------------
# Incoming water
# Get bact names (removing timestamp column)
colnames_bacts <- colnames(incoming_df)[-1]
# Get the sumsbydates
counts_by_dates <- apply(incoming_df[,colnames_bacts], 2, sum)
# Find which counts are zero
count_is_zero <- counts_by_dates == 0
# Make a mask for columns
mask <- c(TRUE, !count_is_zero)
# Incoming water
incoming_vars_df <- incoming_df[, mask]

# Need to save XLSX files?
if (FALSE) {
  for (i in seq(4)) {
    write.xlsx(variables_dfs[[i]], file = paste0("tax_lvl_", taxonomy_level, "_line_", i, ".xlsx"))
  }
}

# Select the columns for modelling
ngs_vars_names <- c("Bacteria,  Acidobacteria,  Acidobacteriia,  Solibacterales",
                    "Bacteria,  Actinobacteria,  Actinobacteria,  Propionibacteriales",
                    "Bacteria,  Proteobacteria,  Gammaproteobacteria,  Legionellales")

ngs_vars_names_plt <- c("`Bacteria,  Acidobacteria,  Acidobacteriia,  Solibacterales`",
                        "`Bacteria,  Actinobacteria,  Actinobacteria,  Propionibacteriales`",
                        "`Bacteria,  Proteobacteria,  Gammaproteobacteria,  Legionellales`")

# Make interpolation models
ngs_data_models <- list()
if (FALSE) {
  
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
}



# Draw all the lines for the same variable on the same plot
if (FALSE) {
  
  for (variable_i in seq(1:length(ngs_vars_names))) {
    
    png(file = paste0("Approxs ", variable_i, ".png"), width = 10, height = 6, units = 'in', res = 300)
    # Plotting
    p <- ggplot(data = NULL, mapping = aes_string(x = "timestamp", y = ngs_vars_names_plt[variable_i]))
    
    for (line_j in seq(1, 4)) {
      
      # Check if there is data
      if (sum(!is.na(variables_dfs[[line_j]][[ngs_vars_names[variable_i]]])) > 1)
      {
        # Data times
        times <- as.numeric(variables_dfs[[line_j]][["timestamp"]])
        min_time <- min(times)
        max_time <- max(times)
        data_spline <- data.frame(seq(min_time, max_time, 1000))
        colnames(data_spline)[1] <- "timestamp"
        data_spline <- data_spline %>%
          mutate(!!ngs_vars_names[variable_i] := ngs_data_models[[line_j]][[variable_i]](timestamp)) %>%
          mutate(timestamp = as.POSIXct(timestamp, origin = '1970-01-01 UTC', tz="UTC"))
        
        # Save to file
        p <- p + 
          geom_point(data = variables_dfs[[line_j]], color = colors_points[line_j], size = 3, shape = 15) +
          scale_x_datetime(date_labels = "%d/%m") +
          geom_point(data = data_spline, size = 1, colour = colors_approx[line_j]) +
          labs(x = "Time", y = ngs_vars_names_plt[variable_i])
      }
      
    }
    
    print(p)
    dev.off()
  }
}
