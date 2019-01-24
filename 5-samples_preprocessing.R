require(xlsx)

# Do we have samples in memory or load from file
have_in_memory <- TRUE

# 
if (have_in_memory)
{
  
  # Set the tax level to load
  tax_level_to_load <- taxonomy_level

  # Read all the data samples and real measurements
  data_samples <- generated_dfs_lines
  data_read <- variables_dfs
  
} else {
  
  # Set the tax level to load
  tax_level_to_load <- 7
  
  # Names of data samples
  data_samples_names <- sapply(seq(4), function(x) {paste0("plane_tax_lvl_", tax_level_to_load, "_line_", x, ".xlsx")})
  # Names of real data
  data_real_names <- sapply(seq(4), function(x) {paste0("real_tax_lvl_", tax_level_to_load, "_line_", x, ".xlsx")})
  
  # Read all the data samples and real measurements
  data_samples <- list()
  data_read <- list()
  for (i_line in seq(4)) {
    line_name <- paste0("Line ", i_line)
    data_samples[[line_name]] <- read.xlsx(data_samples_names[i_line], sheetIndex = 1)
    data_read[[line_name]] <- read.xlsx(data_real_names[i_line], sheetIndex = 1)
  }
  
}

# Use incoming water as input
incoming_as_imput <- TRUE

# 
if (incoming_as_imput) {
  
}
