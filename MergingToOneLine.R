# Mergine the line data

# Setting the lines data
lines_arg <- data
# And the comments for these lines
lines_comment_arg <- comments

# Merged lines
lines <- list()
separate_chunks_lines <- list()
separate_chunks_comments <- list()

# Lines names
line_names <- names(lines_arg)

# For all lines remove elements, which were not loaded corectly
for (i_line in seq(lines_arg)) {
  
  # Find all line elements that were succesfully renamed
  indexes_to_get <- NULL
  
  for (j_df in seq(lines_arg[[i_line]])) {
    
    print(j_df)
    if (lines_comment_arg[[i_line]][[j_df]][["column_renaming"]]) {
      indexes_to_get <- c(indexes_to_get, j_df)
    } else {
      print(paste("There is error with", j_df, "data in line", i_line))
    }
  }
  
  # Get the dfs that are ok
  list_of_proper_dfs <- lines_arg[[i_line]][indexes_to_get]
  separate_chunks_comments[[line_names[[i_line]]]] <- comments[[i_line]][indexes_to_get]
  
  # Merge them
  merged_df <- do.call("rbind", list_of_proper_dfs) %>%
    arrange(timestamp)
  print(nrow(merged_df))
  
  merged_df <- merged_df %>% distinct
  print(nrow(merged_df))
  
  lines[[line_names[[i_line]]]] <- merged_df
  separate_chunks_lines[[line_names[[i_line]]]] <- list_of_proper_dfs
}

source('C:/Users/Ivan/Desktop/sci/DataLoading/NewGapsProcessing.R', echo=TRUE)

# Add gaps to lines
if (TRUE) {
  
  lines_full <- list()
  separate_chunks_lines_full <- list()
  
  for (i_line in seq(lines)) {
    
    lines_full[[line_names[[i_line]]]] <- fill_gaps(data = lines[[i_line]], keep_columns = TRUE, globally = TRUE) %>%
      arrange(timestamp)
    
    # Names of separate dfs
    names_of_dfs <- names(separate_chunks_lines[[i_line]])
    current_line <- list()
    for (j_df in seq(separate_chunks_lines[[i_line]])) {
      current_line[names_of_dfs[j_df]] <- fill_gaps(data = separate_chunks_lines[[i_line]][[j_df]], keep_columns = TRUE, globally = FALSE) %>%
        arrange(timestamp)
    }
    separate_chunks_lines_full[[line_names[[i_line]]]] <- current_line

  }
}

data_to_postporcess <- lines_full