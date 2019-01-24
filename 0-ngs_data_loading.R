options(java.parameters = "-Xmx8000m")

library("tidyverse")
library("xlsx")
library("dplyr")


file_path_map <- "C://Users//Ivan//Desktop//R projects//DWDsome//map.txt"
file_path_table <- "C://Users//Ivan//Desktop//R projects//DWDsome//table.txt"

# Upload the raw data
# For map
raw_df_map <- read.delim(file = file_path_map, stringsAsFactors = FALSE)
raw_df_map_colnames <- colnames(raw_df_map)
# And for table
raw_df_table <- read.delim(file = file_path_table, stringsAsFactors = FALSE)
raw_df_table_colnames <- colnames(raw_df_table)


#'@method Checks if the first symbol is 'X', if true: replace "." by "-" and remove 'X'
check_if_x_first_and_remove <- function(colname) {
  if (substring(colname, 1, 1) == 'X') {
    colname = gsub("\\.", "-", colname)
    return(substring(colname, 2))
  }
  return(colname)
}


# Preprocess the table ----------------------------------------------------------------------
# Remove the first 'X' letter from the colnames
df_table_colnames <- unname(sapply(raw_df_table_colnames, check_if_x_first_and_remove))

# Remove columns which we don't need
df_table_colnames_selector <- !logical(length(df_table_colnames))
df_table_colnames_selector <- setNames(df_table_colnames_selector, df_table_colnames)
df_table_colnames_selector[match("", df_table_colnames)] <- FALSE

# Change the initial data table
df_table <- raw_df_table
colnames(df_table) <- df_table_colnames
df_table <- df_table[, df_table_colnames_selector]
df_table_colnames <- colnames(df_table)
# Preprocess the table ----------------------------------------------------------------------


# Preprocess the map ------------------------------------------------------------------------
# New map table colnames
new_map_colnames <- c(
  "sample_id",
  "code",
  "plate_well",
  "plate",
  "sample_name",
  "template",
  "sampling_date",
  "sampling_point",
  "linja",
  "pipe_material",
  "disinfect_chemical",
  "is_disinfecting",
  "sample_type",
  "total_readcount",
  "lgcoder",
  "lgc_filename")

df_map <- raw_df_map
colnames(df_map) <- new_map_colnames
df_map_colnames <- new_map_colnames
# Preprocess the map ------------------------------------------------------------------------


# Make an extended map
# -----------------------------------------------------------------------
# 
df_map_ext <- df_map
df_map_ext[["is_water"]] <- df_map_ext$sample_type == "water"
df_map_ext[["is_biofilm"]] <- df_map_ext$sample_type == "biofilm"

df_map_ext[["is_cu"]] <- df_map_ext$pipe_material == "Cu"
df_map_ext[["is_pex"]] <- df_map_ext$pipe_material == "PEX"

df_map_ext[["is_dis"]] <- df_map_ext$is_disinfecting == "Yes"
df_map_ext[["is_no_dis"]] <- df_map_ext$is_disinfecting == "No"

df_map_ext[["is_nh2cl"]] <- df_map_ext$disinfect_chemical == "NH2Cl"
df_map_ext[["is_naocl"]] <- df_map_ext$disinfect_chemical == "NaOCl"

df_map_ext[["is_incoming"]] <- df_map_ext$sampling_point == "Incom.water"

df_map_ext[["is_line_1"]] <- df_map_ext$linja == 1
df_map_ext[["is_line_2"]] <- df_map_ext$linja == 2
df_map_ext[["is_line_3"]] <- df_map_ext$linja == 3
df_map_ext[["is_line_4"]] <- df_map_ext$linja == 4

df_map_ext[["is_rna"]] <- tolower(df_map_ext$template) == "cdna"
df_map_ext[["is_dna"]] <- tolower(df_map_ext$template) == "dna"

# Make an extended map
# -----------------------------------------------------------------------