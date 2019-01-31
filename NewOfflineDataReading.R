require(readxl)
require(dplyr)

# Reading the offline data

# Load the data
path <- "C:/Users/Ivan/Desktop/sci/DataLoading/offline_data.xlsx"
all_offline_data <- read_excel(path = path, sheet = "Data")

# Choose the columns we are going to use in the analysis
columns_to_keep_offline <- c("Samplingdate",
                             "Samplingpoint",
                             "Linja (1, 2, 3, 4, SIS)",
                             "Pipematerial",
                             "Sampletype (water/biofilm)",
                             "Temperature ('C) field",
                             "pH",
                             "EC (uS/cm)",
                             "Cu (mg/l)",
                             "Fe (mg/l)",
                             "Cholrine total (mg/l)",
                             "Chlorine free (mg/l)",
                             "Chloroamine (mg/l)",
                             "R2A (cfu/ml)",
                             "Turbidity",
                             "DAPI",
                             "ATP")

# Rename the columns
columns_new_names <- c("timestamp", 
                       "samplingpoint",
                       "line",
                       "pipematerial",
                       "sampletype",
                       "temperature",
                       "ph",
                       "specific_conductance",
                       "cu",
                       "fe",
                       "chlorine_total",
                       "chlorine_free",
                       "chloramine",
                       "r2a",
                       "turbidity",
                       "dapi",
                       "atp")

# Variable names to work with
ext_offline_variables <- c("temperature",
                           "ph",
                           "specific_conductance",
                           "cu",
                           "fe",
                           "chlorine_total",
                           "chlorine_free",
                           "chloramine",
                           "r2a",
                           "turbidity",
                           "dapi",
                           "atp")

# Get the needed values in sample point
sample_points <- unique(all_offline_data$Samplingpoint)[grepl("ULOS", unique(all_offline_data$Samplingpoint))]
# Filter out this data
all_offline_data <- all_offline_data %>% subset(Samplingpoint %in% sample_points)

# Select the data by the column names
selected_offline_df <- all_offline_data %>%
  select(one_of(columns_to_keep_offline))

# Rename the initial dataframe
colnames(selected_offline_df) <- columns_new_names

# Make the columns numeric
for (colname in ext_offline_variables) {
  selected_offline_df[[colname]] <- as.numeric(selected_offline_df[[colname]])
}

# Disinfection timestamp
disinfection_start <- as.POSIXct("02.08.2018", format = "%d.%m.%Y", origin = "01.01.1970", tz = "UTC")

# Make a lines
ext_offline_list <- list()
for (line_i in seq(4)) {
  # Filter a data for particular line
  ext_offline_list[[line_i]] <- selected_offline_df %>% 
    filter_(.dots = paste0("line == ", line_i)) %>%
    arrange(timestamp) %>%
    mutate(chlorine_total = ifelse(is.na(chlorine_total) & timestamp < disinfection_start, 0, chlorine_total)) %>%
    mutate(chlorine_free = ifelse(is.na(chlorine_free) & timestamp < disinfection_start, 0, chlorine_free)) %>%
    mutate(chloramine = ifelse(is.na(chloramine) & timestamp < disinfection_start, 0, chloramine))
}

# Add the inputs

# Inputs timestampts
inputs_changing_timestamps <- list(
  as.POSIXct("02.08.2017 11:55", format = "%d.%m.%Y %H:%M", origin = "01.01.1970", tz = "UTC"),
  as.POSIXct("03.08.2017 15:30", format = "%d.%m.%Y %H:%M", origin = "01.01.1970", tz = "UTC"),
  as.POSIXct("14.08.2017 13:50", format = "%d.%m.%Y %H:%M", origin = "01.01.1970", tz = "UTC"),
  as.POSIXct("16.08.2017 14:30", format = "%d.%m.%Y %H:%M", origin = "01.01.1970", tz = "UTC"),
  as.POSIXct("17.08.2017 16:55", format = "%d.%m.%Y %H:%M", origin = "01.01.1970", tz = "UTC")
)
inputs_changing_timestamps <- unlist(lapply(inputs_changing_timestamps, as.numeric))


# Inputs values
# Chlorine
input_chlorine <- c(
  0.25,
  0.17,
  0.21,
  0.25,
  0.33
)
# Ammonium
input_ammonium <- c(
  0.075,
  0.050,
  0.063,
  0.075,
  0.100
)
# Make a df of inputs
inputs_df <- data.frame(timestamp = as.POSIXct(inputs_changing_timestamps, format = "%d.%m.%Y", origin = "01.01.1970", tz = "UTC"), 
                        chlorine = input_chlorine, 
                        ammonium = input_ammonium)