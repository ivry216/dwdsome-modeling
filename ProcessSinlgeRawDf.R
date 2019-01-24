# --------------------------------------------------------------------
# Constants

# Colnames for lines 1 and 2
colnames_lines_a <- c("timestamp", 
                      "conductivity", 
                      "specific_conductance",
                      "dissolved_oxygen",
                      "orp",
                      "ph",
                      "temperature",
                      "comment",
                      "site",
                      "folder",
                      "unit_id")

colnames_a_count <- length(colnames_lines_a)

line_1_id <- "THL3403"
line_2_id <- "THL3404"

# Colnames for lines 3 and 4
colnames_lines_b <- c("timestamp",
                      "temperature",
                      "specific_conductance",
                      "sal_ppt",
                      "depth",
                      "ph",
                      "ph_mv",
                      "orp",
                      "turbid",
                      "odosat",
                      "dissolved_oxygen",
                      "battery")
colnames_b_count <- length(colnames_lines_b)

get_columns_a_lines <- c("Timestamp",
                         "Conductivity (mS/cm)",
                         "Specific Conductance (uS/cm)",
                         "Dissolved Oxygen (mg/L)",
                         "ORP_1 (mV)",
                         "pH_1 (Units)",
                         "Temperature (C)",
                         "Comment",
                         "Site",
                         "Folder",
                         "Unit ID")

get_columns_b_lines <- c("Date Time M/D/Y HH:MM:SS",
                         "Temp C",
                         "SpCond uS",
                         "Sal ppt",
                         "Depth meters",
                         "pH",
                         "pH mV",
                         "Orp mV",
                         "Turbid+ NTU",
                         "ODOsat %",
                         "ODO mg/L",
                         "Battery volts")
# --------------------------------------------------------------------



# --------------------------------------------------------------------
# 0. Copy the data to work with
data <- list_of_data
line_names <- names(list_of_data)

comments <- list()
# --------------------------------------------------------------------
# 1. Delete unwanted columns

# Symbols of unwanted columns
pattern_unwanted_cols <- "X_"
# Method checking if all names are found
all_names_are_found <- function(names, proper_names) {
  checker <- NULL
  for (i in seq(proper_names)) {
    checker <- c(checker, proper_names[i] %in% names)
  }
  return(all(checker))
}

line_1_in_2 <- list()
line_2_in_1 <- list()

# For all lines
for (line_i in seq(data)) {
  
  coments_line <- list()
  df_names <- names(data[[line_i]])
  
  # For all df for current line
  for (df_j in seq(data[[line_i]])) {
    
    comments_df <- list()
    
    this_df <- data[[line_i]][[df_j]]
    # Get the colnames of the df
    this_colnames <- colnames(this_df)
    # Find columns that are not matching pattern
    cols_to_select <- !grepl(pattern_unwanted_cols, this_colnames)
    # Now select only required columns and save it to data
    this_df <- this_df[,cols_to_select]
    
    if (line_i %in% c(1,2)) {
      # Check if all names were found
      is_all_found <- all_names_are_found(this_colnames, get_columns_a_lines)
      
      # If it is the first line, find if it contains the second
      if (line_i == 1) {
        look_for_2 <- this_df %>%
          filter(`Unit ID` == line_2_id)
        
        if (!is.null(look_for_2) && nrow(look_for_2) > 0) {
          line_2_in_1[[df_names[[df_j]]]] <- look_for_2
          this_df <- this_df %>% 
            filter(`Unit ID` == line_1_id)
        }
      }
      
      # If it is the second line, find if it contains the first
      if (line_i == 2) {
        look_for_1 <- this_df %>%
          filter(`Unit ID` == line_1_id)
        
        if (!is.null(look_for_1) && nrow(look_for_1) > 0) {
          line_1_in_2[[df_names[[df_j]]]] <- look_for_1
          this_df <- this_df %>% 
            filter(`Unit ID` == line_2_id)
        }
      }
      
      if (is_all_found) {
        this_df <- this_df %>% select(get_columns_a_lines)
        comments_df[["proper_cols_found"]] <- TRUE
      } else {
        comments_df[["proper_cols_found"]] <- FALSE
      }
      
    } else if (line_i %in% c(3,4)) {
      # Check if all names were found
      is_all_found <- all_names_are_found(this_colnames, get_columns_b_lines)
      
      if (is_all_found) {
        this_df <- this_df %>% select(get_columns_b_lines)
        comments_df[["proper_cols_found"]] <- TRUE
      } else {
        comments_df[["proper_cols_found"]] <- FALSE
      }
    }
    
    comments_df[["is_from_other_line"]] <- FALSE
    
    data[[line_i]][[df_j]] <- this_df
    coments_line[[df_names[[df_j]]]] <- comments_df
  }
  
  comments[[line_names[[line_i]]]] <- coments_line
}

# --------------------------------------------------------------------
# 1.1 Add the doubling data to the data

# Line 1 found in line 2
# Its names
line_1_in_2_names <- names(line_1_in_2)
# Add all to line 2
for (i_1 in seq(line_1_in_2)) {
  data[[1]][[line_1_in_2_names[[i_1]]]] <- line_1_in_2[[i_1]]
  comments[[1]][[line_1_in_2_names[[i_1]]]] <- comments[[2]][[line_1_in_2_names[[i_1]]]]
  comments[[1]][[line_1_in_2_names[[i_1]]]][["is_from_other_line"]] <- TRUE
}

# Line 2 found in line 1
# Its names
line_2_in_1_names <- names(line_2_in_1)
# Add all to line 2
for (i_2 in seq(line_2_in_1)) {
  data[[2]][[line_2_in_1_names[[i_2]]]] <- line_2_in_1[[i_2]]
  comments[[2]][[line_2_in_1_names[[i_2]]]] <- comments[[1]][[line_2_in_1_names[[i_2]]]]
  comments[[2]][[line_2_in_1_names[[i_2]]]][["is_from_other_line"]] <- TRUE
}

# --------------------------------------------------------------------
# 2. Rename the rest colnames

# For all lines
for (line_i in seq(data)) {
  
  # For all df for current line
  for (df_j in seq(data[[line_i]])) {
    
    this_df <- data[[line_i]][[df_j]]
    this_colnames <- colnames(this_df)
    
    # For first pair of lines
    if (line_i %in% c(1, 2)) {
      # Check if number of columns is correct
      if (length(this_colnames) == colnames_a_count) {
        colnames(this_df) <- colnames_lines_a
        data[[line_i]][[df_j]] <- this_df
        comments[[line_i]][[df_j]][["column_renaming"]] <- TRUE
      } else {
        comments[[line_i]][[df_j]][["column_renaming"]] <- FALSE
      }
    }
    
    # For second pair of lines
    if (line_i %in% c(3, 4)) {
      # Check if number of columns is correct
      if (length(this_colnames) == colnames_b_count) {
        colnames(this_df) <- colnames_lines_b
        data[[line_i]][[df_j]] <- this_df
        comments[[line_i]][[df_j]][["column_renaming"]] <- TRUE
      } else {
        comments[[line_i]][[df_j]][["column_renaming"]] <- FALSE
      }
    }
  }
}

# --------------------------------------------------------------------
# 3. Make correct types
# Types of classes of the first pair of lines
types_a_lines <- c("POSIXct",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "character",
                   "character",
                   "character",
                   "character")

# Types of classes of the second pair of lines
types_b_lines <- c("POSIXct",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric")

# Function that makes a parsing
parse_df_columns <- function(df, classes, proper_classes) {
  for (i in seq(classes)) {
    if (!(proper_classes[i] %in% unlist(classes[i]))) {
      if ((proper_classes[i] == "POSIXct") & !("POSIXlt" %in% unlist(classes[i]))) {
        df[[i]] <- strptime(df[[i]], "%d.%m.%Y %H:%M:%S")
      } else if (proper_classes[i] == "numeric") {
        df[[i]] <- as.numeric(df[[i]])
      }
    }
  }
  return(df)
}


for (i_line in seq(data)) {
  
  df_names <- names(data[[i_line]])
  
  for (j_df in seq(data[[i_line]])) {
    
    print(df_names[j_df])
    if (comments[[i_line]][[j_df]][["column_renaming"]]) {
      if (i_line %in% c(1,2)) {
        this_df <- data[[i_line]][[j_df]]
        classes <- sapply(this_df, class)
        
        df <- parse_df_columns(this_df, classes, types_a_lines)
        data[[i_line]][[j_df]] <- df
        
        comments[[i_line]][[j_df]][["summary"]] <- summary(df)
        
      } else if (i_line %in% c(3,4)) {
        this_df <- data[[i_line]][[j_df]]
        classes <- sapply(this_df, class)
        
        df <- parse_df_columns(this_df, classes, types_b_lines)
        data[[i_line]][[j_df]] <- df
        
        comments[[i_line]][[j_df]][["summary"]] <- summary(df)
      }
    }
  }
}

# --------------------------------------------------------------------
# 4. Process the data and rename the df in data list
for (i_line in seq(data)) {
  
  df_names <- names(data[[i_line]])
  
  for (j_df in seq(data[[i_line]])) {
    
    print(df_names[j_df])
    if (comments[[i_line]][[j_df]][["column_renaming"]]) {
      
      if (i_line %in% c(1,2)) {
        
        min_date <- min(data[[i_line]][[j_df]][["timestamp"]])
        max_date <- max(data[[i_line]][[j_df]][["timestamp"]])
        min_day <- day(min_date)
        min_month <- month(min_date)
        max_day <- day(max_date)
        max_month <- month(max_date)
        print(paste0(min_day,".",min_month," - ",max_day,".",max_month))
        names(data[[i_line]])[j_df] <- paste0(min_day,".",min_month," - ",max_day,".",max_month)
        
        this_classes <- lapply(data[[i_line]][[j_df]], class)
        this_indexes <- unlist(lapply(this_classes, function(x) {!any(grepl("character|logical", x))}))
        data[[i_line]][[j_df]] <- data[[i_line]][[j_df]][,this_indexes]
        
        # Cutting temperature
        data[[i_line]][[j_df]][["temperature"]][(abs(data[[i_line]][[j_df]][["temperature"]]) > 30) | 
                                                  data[[i_line]][[j_df]][["temperature"]] < 0] <- NA
        
        # Cutting conductivity
        data[[i_line]][[j_df]][["conductivity"]][(abs(data[[i_line]][[j_df]][["conductivity"]]) > 5)] <- NA
        
        # Cutting specific_conductance
        data[[i_line]][[j_df]][["specific_conductance"]][data[[i_line]][[j_df]][["specific_conductance"]] > 300 |
                                                           data[[i_line]][[j_df]][["specific_conductance"]] < 100] <- NA

        # Cutting dissolved_oxygen
        data[[i_line]][[j_df]][["dissolved_oxygen"]][abs(data[[i_line]][[j_df]][["dissolved_oxygen"]]) > 15] <- NA

        # Cutting orp
        data[[i_line]][[j_df]][["orp"]][abs(data[[i_line]][[j_df]][["orp"]]) > 1000] <- NA

        # Cutting ph
        data[[i_line]][[j_df]][["ph"]][data[[i_line]][[j_df]][["ph"]] > 9 | 
                                         data[[i_line]][[j_df]][["ph"]] < 7] <- NA

        # Make all datetimes floored by 5 mins
        data[[i_line]][[j_df]][["timestamp"]] <- floor_date(data[[i_line]][[j_df]][["timestamp"]], "5 mins")
          
        names(comments[[i_line]])[j_df] <- paste0(min_day,".",min_month," - ",max_day,".",max_month)
        trial <- summary(data[[i_line]][[j_df]])
        
        comments[[i_line]][[j_df]][["summary"]] <- trial
        comments[[i_line]][[j_df]][["start"]] <- min(data[[i_line]][[j_df]][["timestamp"]])
        comments[[i_line]][[j_df]][["end"]] <- max(data[[i_line]][[j_df]][["timestamp"]])
        
      } else if (i_line %in% c(3,4)) {
        
        min_date <- min(data[[i_line]][[j_df]][["timestamp"]])
        max_date <- max(data[[i_line]][[j_df]][["timestamp"]])
        min_day <- day(min_date)
        min_month <- month(min_date)
        max_day <- day(max_date)
        max_month <- month(max_date)
        print(paste0(min_day,".",min_month," - ",max_day,".",max_month))
        names(data[[i_line]])[j_df] <- paste0(min_day,".",min_month," - ",max_day,".",max_month)
        
        this_classes <- lapply(data[[i_line]][[j_df]], class)
        this_indexes <- unlist(lapply(this_classes, function(x) {!any(grepl("character|logical", x))}))
        data[[i_line]][[j_df]] <- data[[i_line]][[j_df]][,this_indexes]
        
        # Cutting temperature
        data[[i_line]][[j_df]][["temperature"]][(abs(data[[i_line]][[j_df]][["temperature"]]) > 30) | 
                                                  data[[i_line]][[j_df]][["temperature"]] < 0] <- NA
        
        if (i_line == 4) {
          start_dropping <- yday(as.POSIXct("01.08.2018", format="%d.%m.%Y"))
          end_dropping <- yday(as.POSIXct("10.08.2018", format="%d.%m.%Y"))
          print(sum(yday(data[[i_line]][[j_df]][["timestamp"]]) > start_dropping & yday(data[[i_line]][[j_df]][["timestamp"]]) < end_dropping))
          data[[i_line]][[j_df]][["temperature"]][yday(data[[i_line]][[j_df]][["timestamp"]]) > start_dropping &
                                                    yday(data[[i_line]][[j_df]][["timestamp"]]) < end_dropping] <- NA
          data[[i_line]][[j_df]][["ph"]][yday(data[[i_line]][[j_df]][["timestamp"]]) > start_dropping &
                                                    yday(data[[i_line]][[j_df]][["timestamp"]]) < end_dropping] <- NA
          data[[i_line]][[j_df]][["specific_conductance"]][yday(data[[i_line]][[j_df]][["timestamp"]]) > start_dropping &
                                           yday(data[[i_line]][[j_df]][["timestamp"]]) < end_dropping] <- NA
        }
        
        # Cutting specific_conductance
        data[[i_line]][[j_df]][["specific_conductance"]][data[[i_line]][[j_df]][["specific_conductance"]] > 300 |
                                                           data[[i_line]][[j_df]][["specific_conductance"]] < 100] <- NA
        
        # Cutting sal_ppt
        data[[i_line]][[j_df]][["sal_ppt"]][abs(data[[i_line]][[j_df]][["sal_ppt"]]) > 1] <- NA
        
        # Cutting depth
        data[[i_line]][[j_df]][["depth"]][abs(data[[i_line]][[j_df]][["depth"]]) > 1] <- NA
        
        # Cutting ph
        data[[i_line]][[j_df]][["ph"]][data[[i_line]][[j_df]][["ph"]] > 9 | 
                                         data[[i_line]][[j_df]][["ph"]] < 7] <- NA
        
        # Cutting ph_mv
        data[[i_line]][[j_df]][["ph_mv"]][abs(data[[i_line]][[j_df]][["ph_mv"]]) > 1000] <- NA
        
        # Cutting orp
        data[[i_line]][[j_df]][["orp"]][data[[i_line]][[j_df]][["orp"]] < -150] <- NA
        
        # Cutting turbid
        data[[i_line]][[j_df]][["turbid"]][abs(data[[i_line]][[j_df]][["turbid"]]) > 250] <- NA
        
        # Cutting odosat
        data[[i_line]][[j_df]][["odosat"]][abs(data[[i_line]][[j_df]][["odosat"]]) > 150] <- NA

        # Cutting dissolved_oxygen
        data[[i_line]][[j_df]][["dissolved_oxygen"]][data[[i_line]][[j_df]][["dissolved_oxygen"]] > 15 |
                                                       data[[i_line]][[j_df]][["dissolved_oxygen"]] < 1] <- NA

        # Cutting battery
        data[[i_line]][[j_df]][["battery"]][abs(data[[i_line]][[j_df]][["battery"]]) > 20] <- NA
        
        # Make all datetimes floored by 5 mins
        data[[i_line]][[j_df]][["timestamp"]] <- floor_date(data[[i_line]][[j_df]][["timestamp"]], "5 mins")
        
        names(comments[[i_line]])[j_df] <- paste0(min_day,".",min_month," - ",max_day,".",max_month)
        trial <- summary(data[[i_line]][[j_df]])
        
        comments[[i_line]][[j_df]][["summary"]] <- trial
        comments[[i_line]][[j_df]][["start"]] <- min(data[[i_line]][[j_df]][["timestamp"]])
        comments[[i_line]][[j_df]][["end"]] <- max(data[[i_line]][[j_df]][["timestamp"]])
        
      }
    }
  }
}
