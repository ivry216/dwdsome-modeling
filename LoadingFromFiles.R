# Set the JRE
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_191') # (for 64-bit version)

# Set locale
Sys.setlocale("LC_ALL", "finnish")

# Define options for JVM
options(java.parameters = "-Xmx4g")

# Upload libraries
library("XLConnectJars")
library("XLConnect")
library("tidyverse")
library("readxl")
library("lubridate")
library("scales")
library("pracma")
library("zoo")
library("tidyquant")

# Set path to read the xl file
path <- "C:/Users/Ivan/Desktop/sci/DataLoading/WP4_pilot"

# Find all the subdirectories
sub_path <- list.dirs(path = path, full.names = TRUE, recursive = FALSE)

# Get subdirectories, containing word "Linja"
word <- "linja"
sub_path <- sub_path[grepl(word, tolower(sub_path))]

# Get subfolders names
subpaths_names <- unname(sapply(sub_path, basename))

# Get subfolders to extract
subpaths_list <- list()
sub_subpath_names <- list()
# For all subfolders
for (i in seq(sub_path)) {
  
  sub_sub_path <- list.dirs(path = sub_path[i], full.names = TRUE, recursive = FALSE)
  subpaths_list[[subpaths_names[i]]] <- sub_sub_path
  sub_subpath_names[[subpaths_names[i]]] <- unname(sapply(sub_sub_path, basename))
  
}

# String pattern for the date in the beginning 
data_pattern <- "[0-9]+\\.[0-9]+\\.[0-9]+"

# Select only those who starts with the data
for (i in seq(sub_path)) {
  
  subpaths_list[[i]] <- subpaths_list[[i]][grepl(data_pattern, sub_subpath_names[[i]])]
  sub_subpath_names[[i]] <- sub_subpath_names[[i]][grepl(data_pattern, sub_subpath_names[[i]])]
  
}

# Get all the files from the folders
path_files_list <- list()
path_files_names_list <- list()

for (i in seq(sub_path)) {
  
  path_files_sublist <- list()
  path_files_names_sublist <- list()
  
  for (j in seq(subpaths_list[[i]])) {
    
    # Path of the files
    path_files_sublist[[sub_subpath_names[[i]][j]]] <- list.files(
      path = subpaths_list[[i]][[j]], 
      full.names = TRUE, 
      recursive = FALSE)
    
    # Names of the files
    path_files_names_sublist[[sub_subpath_names[[i]][j]]] <- unname(
      sapply(path_files_sublist[[sub_subpath_names[[i]][j]]], basename))
    
  }
  
  path_files_list[[subpaths_names[[i]]]] <- path_files_sublist
  path_files_names_list[[subpaths_names[[i]]]] <- path_files_names_sublist
  
}


pattern_xlsx <- "\\.xlsx"
pattern_xls <- "\\.xls"
pattern_csv <- "\\.csv"

extension_patterns <- c(pattern_xlsx, pattern_xls, pattern_csv) 

name_xlsx <- "xlsx"
name_xls <- "xls"
name_csv <- "csv"

extension_names <- c(name_xlsx, name_xls, name_csv)

# 1, 2, 3 - opening XLSX, XLS or CSV

#' ------------------------------------------------------------------------------------------
#' @method Method for identifying if the filename contain one of the pattern
#' ------------------------------------------------------------------------------------------

which_file <- function(filename, extension_patterns, extention_names) {
  
  for (i in seq(extension_patterns)) {
    if (grepl(extension_patterns[[i]], filename)) {
      return(extention_names[[i]])
    }
  }
  
  return(NA)
  
}

#' ------------------------------------------------------------------------------------------



#' ------------------------------------------------------------------------------------------
#' @method Method returns the extension type, filename and its path
#' ------------------------------------------------------------------------------------------

what_to_load <- function(list_of_list_of_filenames, list_of_list_of_paths, extension_patterns, extention_names) {
  
  result <- list()
  
  # Get Linja names
  linja_names <- names(list_of_list_of_filenames)
  
  for (linja in seq(list_of_list_of_filenames)) {
    
    linja_list <- list()
    
    # Get forlder names 
    folder_names <- names(list_of_list_of_filenames[[linja]])
    
    for (folder in seq(list_of_list_of_filenames[[linja]])) {
      
      is_found <- FALSE
      folder_list <- list()
      
      for (filename in seq(list_of_list_of_filenames[[linja]][[folder]])) {
        
        this_path <- list_of_list_of_paths[[linja]][[folder]][[filename]]
        this_name <- list_of_list_of_filenames[[linja]][[folder]][[filename]]
        
        type <- which_file(
          this_name,
          extension_patterns,
          extention_names)
        
        if (!is.na(type)) {
          
          file_list <- list()
          file_list[["name"]] <- this_name
          file_list[["path"]] <- this_path
          file_list[["type"]] <- type
          
          folder_list[[this_name]] <- file_list
          
          is_found <- TRUE
        }
        
      }
      
      if (!is_found) {
        linja_list[[folder_names[[folder]]]] <- NA
      } else {
        linja_list[[folder_names[[folder]]]] <- folder_list
      }
      
    }
    
    result[[linja_names[[linja]]]] <- linja_list
    
  }
  return(result)
}

#' ------------------------------------------------------------------------------------------

loading_lists <- what_to_load(
  list_of_list_of_filenames = path_files_names_list,
  list_of_list_of_paths = path_files_list,
  extension_patterns = extension_patterns,
  extention_names = extension_names
)

# Check if everything is loaded correct

for (i in seq(loading_lists)) {
  for (j in seq(loading_lists[[i]])) {
    if (is.na(loading_lists[[i]][[j]])) {
      print(paste(i, "line and", path_files_list[[i]][[j]], "folder are not found"))  
    }
  }
}

#' ------------------------------------------------------------------------------------------


list_of_data <- list()

linja_names <- names(loading_lists)

for (i_line in seq(loading_lists)) {
  
  linja_data <- list()
  
  for (j_folder in seq(loading_lists[[i_line]])) {
    
    if (length(loading_lists[[i_line]][[j_folder]]) > 1) {
      print(paste0("Many elements: ","Line ", i_line, ", ", names(loading_lists[[i_line]])[[j_folder]]))
    }
    
    if (!is.na(loading_lists[[i_line]][[j_folder]])) {
      
      for (k_file in seq(loading_lists[[i_line]][[j_folder]])) {
        # Load the xlsx files
        if (loading_lists[[i_line]][[j_folder]][[k_file]]$type == name_xlsx) {
          print(paste(loading_lists[[i_line]][[j_folder]][[k_file]]$type, " ", linja_names[i_line], loading_lists[[i_line]][[j_folder]][[k_file]]$name))
          linja_data[[loading_lists[[i_line]][[j_folder]][[k_file]]$name]] <- read_excel(path = loading_lists[[i_line]][[j_folder]][[k_file]]$path)
          print(dim(linja_data[[loading_lists[[i_line]][[j_folder]][[k_file]]$name]]))
        }
        # Load xls files
        if (loading_lists[[i_line]][[j_folder]][[k_file]]$type == name_xls) {
          print(paste(loading_lists[[i_line]][[j_folder]][[k_file]]$type, " ", linja_names[i_line], loading_lists[[i_line]][[j_folder]][[k_file]]$name))
          linja_data[[loading_lists[[i_line]][[j_folder]][[k_file]]$name]] <- read.csv(file = loading_lists[[i_line]][[j_folder]][[k_file]]$path, sep=';', header = TRUE, stringsAsFactors = FALSE)
          print(dim(linja_data[[loading_lists[[i_line]][[j_folder]][[k_file]]$name]]))
        }
        # Load csv files
        if (loading_lists[[i_line]][[j_folder]][[k_file]]$type == name_csv) {
          print(paste(loading_lists[[i_line]][[j_folder]][[k_file]]$type, " ", linja_names[i_line], loading_lists[[i_line]][[j_folder]][[k_file]]$name))
          linja_data[[loading_lists[[i_line]][[j_folder]][[k_file]]$name]] <- read.csv(file = loading_lists[[i_line]][[j_folder]][[k_file]]$path, sep=';', header = TRUE, stringsAsFactors = FALSE)
          print(dim(linja_data[[loading_lists[[i_line]][[j_folder]][[k_file]]$name]]))
        }
      }
    }
  }
  
  list_of_data[[linja_names[[i_line]]]] <- linja_data
  
}
