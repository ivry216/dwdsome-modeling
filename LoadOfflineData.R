# Reading the offline data

# Load the data
path <- "C:/Users/Ivan/Desktop/sci/DataLoading/offline_data.xlsx"
offline_data <- read_excel(path = path, sheet = "Data")

# Get the needed values in sample point
sample_points <- unique(offline_data$Samplingpoint)[grepl("ULOS", unique(offline_data$Samplingpoint))]
sis_points <- unique(offline_data$Samplingpoint)[grepl("Sis", unique(offline_data$Samplingpoint))]

# Filter the SIS data
sis_data <- offline_data %>% subset(Samplingpoint %in% sis_points)

# Filter out this data
offline_data <- offline_data %>% subset(Samplingpoint %in% sample_points)

# Change the names
offline_colnames <- colnames(offline_data)
# Rename some
offline_colnames[grepl("Temperature ('C) field", offline_colnames, fixed = TRUE)] <- "temperature"
offline_colnames[grepl("Samplingdate", offline_colnames, fixed = TRUE)] <- "timestamp"
offline_colnames[grepl("pH", offline_colnames, fixed = TRUE)] <- "ph"
offline_colnames[grepl("EC (uS/cm)", offline_colnames, fixed = TRUE)] <- "specific_conductance"
colnames(offline_data) <- offline_colnames
colnames(sis_data) <- offline_colnames

# The colnames we will select
required_colnames <- c("timestamp", "temperature", "ph", "specific_conductance")

# Rename the lines (find the digits in the line names)
new_line_column <- unname(sapply(offline_data$Samplingpoint, function(x) {as.numeric(gsub("\\D", "", x))}))

# Apply these column data to the raw column
offline_data$Samplingpoint <- new_line_column

# Perform the list of offline data
offline_data_list <- list()
line_names <- c("Linja 1", "Linja 2", "Linja 3", "Linja 4")
for (i in seq(line_names)) {
  offline_data_list[[line_names[i]]] <- offline_data %>%
    filter(Samplingpoint == i) %>%
    mutate(Line = i) %>%
    select(one_of(c(required_colnames, "Line"))) %>%
    mutate(Line = as.factor(Line)) %>%
    mutate(temperature = as.numeric(temperature)) %>%
    mutate(ph = as.numeric(ph)) %>%
    mutate(specific_conductance = as.numeric(specific_conductance)) %>%
    arrange(timestamp)
}

# SIS data processing
sis_data <- sis_data %>%
  mutate(temperature = as.numeric(temperature)) %>%
  mutate(ph = as.numeric(ph)) %>%
  mutate(specific_conductance = as.numeric(specific_conductance))

# Columns to select
sis_cols_to_select <- c("timestamp", "temperature", "ph", "specific_conductance")
# Get clean water
sis_clean <- sis_data %>%
  filter(Samplingpoint == "Sis") %>%
  select(one_of(sis_cols_to_select)) %>%
  arrange(timestamp)
# Get Amiini
sis_amiini <- sis_data %>%
  filter(Samplingpoint == "SisAmiini") %>%
  select(one_of(sis_cols_to_select)) %>%
  arrange(timestamp)
# Get Hypo
sis_hypo <- sis_data %>%
  filter(Samplingpoint == "SisHypo") %>%
  select(one_of(sis_cols_to_select)) %>%
  arrange(timestamp)