# Load required libraries
library(readxl)
library(dplyr)
library(readr)

# Set working directory
setwd("C:/Users/Gairui/OneDrive - University College London/Housing policy VRA/Processed")

# Read existing combined data (2011-2016)
combined_data <- read_csv("combined_lha_2011_2016.csv")

# Function to process LHA data from 2017-2018 Excel files
# Format: 'BRMA', 'Room', '1 Bed', '2 Bed', '3 Bed', '4 Bed'
process_lha_excel_2017_2018 <- function(file_path, year, sheet_name) {
  # Read Excel data, skipping header rows
  lha_data <- read_excel(file_path, sheet = sheet_name, skip = 2, col_names = FALSE)
  
  # Print dimensions for verification
  cat("Dimensions of data from", file_path, ":", dim(lha_data)[1], "rows,", dim(lha_data)[2], "columns\n")
  
  # Select only the first 6 columns and rename them
  lha_data <- lha_data[, 1:6]
  colnames(lha_data) <- c(
    "Region",
    "Shared", 
    "1 Bedroom", 
    "2 Bedrooms", 
    "3 Bedrooms", 
    "4 Bedrooms"
  )
  
  # Add year column
  lha_data$Year <- year
  
  # Add 5 Bedrooms column with NA values
  lha_data$`5 Bedrooms` <- NA
  
  # Reorder columns to match the combined dataset
  lha_data <- lha_data %>%
    select(Region, Year, Shared, `1 Bedroom`, `2 Bedrooms`, `3 Bedrooms`, `4 Bedrooms`, `5 Bedrooms`)
  
  return(lha_data)
}

# Function to process LHA data from 2019-2024 Excel files
# Format: 'BRMA', 'CAT A', 'CAT B', 'CAT C', 'CAT D', 'CAT E'
process_lha_excel_2019_2024 <- function(file_path, year, sheet_name) {
  # Read Excel data, skipping header rows
  lha_data <- read_excel(file_path, sheet = sheet_name, skip = 2, col_names = FALSE)
  
  # Print dimensions for verification
  cat("Dimensions of data from", file_path, ":", dim(lha_data)[1], "rows,", dim(lha_data)[2], "columns\n")
  
  # Select only the first 6 columns and rename them
  lha_data <- lha_data[, 1:6]
  colnames(lha_data) <- c(
    "Region",
    "Shared",      # CAT A
    "1 Bedroom",   # CAT B
    "2 Bedrooms",  # CAT C
    "3 Bedrooms",  # CAT D
    "4 Bedrooms"   # CAT E
  )
  
  # Add year column
  lha_data$Year <- year
  
  # Add 5 Bedrooms column with NA values
  lha_data$`5 Bedrooms` <- NA
  
  # Reorder columns to match the combined dataset
  lha_data <- lha_data %>%
    select(Region, Year, Shared, `1 Bedroom`, `2 Bedrooms`, `3 Bedrooms`, `4 Bedrooms`, `5 Bedrooms`)
  
  return(lha_data)
}

# Check region consistency function
check_region_consistency <- function(data) {
  regions_by_year <- data %>%
    group_by(Year) %>%
    summarise(regions = list(sort(unique(Region))), .groups = 'drop')
  
  # Print counts of unique regions by year
  cat("Number of unique regions by year:\n")
  regions_by_year %>%
    mutate(count = sapply(regions, length)) %>%
    select(Year, count) %>%
    as.data.frame() %>%
    print()
  
  # Find any regions that don't appear in all years
  all_regions <- sort(unique(data$Region))
  years <- sort(unique(data$Year))
  
  cat("\nChecking for regions that don't appear in all years...\n")
  for (region in all_regions) {
    years_present <- data %>% 
      filter(Region == region) %>% 
      pull(Year) %>% 
      unique() %>%
      sort()
    
    if (length(years_present) < length(years)) {
      missing_years <- setdiff(years, years_present)
      cat(sprintf("Region '%s' is missing in years: %s\n", 
                  region, paste(missing_years, collapse = ", ")))
    }
  }
}

##### PART 1: Process 2017 and 2018 data #####

# Process 2017 and 2018 data
cat("Processing 2017 data...\n")
lha_2017 <- process_lha_excel_2017_2018("LHA_2017.xlsx", 2017, "Table 5")

cat("Processing 2018 data...\n")
lha_2018 <- process_lha_excel_2017_2018("LHA_2018.xlsx", 2018, "Table 4")

# Print sample of processed data to verify
cat("\nSample of 2017 data:\n")
print(head(lha_2017, 3))

cat("\nSample of 2018 data:\n")
print(head(lha_2018, 3))

# Combine with existing data
updated_combined_data <- bind_rows(
  combined_data,
  lha_2017,
  lha_2018
)

# Convert any string "NA" values to actual NA values
updated_combined_data <- updated_combined_data %>%
  mutate(across(where(is.character), ~ifelse(. == "NA", NA, .)))

# Ensure consistent data types across all columns
updated_combined_data <- updated_combined_data %>%
  mutate(
    Region = as.character(Region),
    Year = as.integer(Year),
    Shared = as.numeric(Shared),
    `1 Bedroom` = as.numeric(`1 Bedroom`),
    `2 Bedrooms` = as.numeric(`2 Bedrooms`),
    `3 Bedrooms` = as.numeric(`3 Bedrooms`),
    `4 Bedrooms` = as.numeric(`4 Bedrooms`),
    `5 Bedrooms` = as.numeric(`5 Bedrooms`)
  )

# Check region consistency
check_region_consistency(updated_combined_data)

# Sort the updated combined dataset by Region and Year
updated_combined_data <- updated_combined_data %>%
  arrange(Region, Year)

# Save the updated combined dataset in multiple formats
write_csv(updated_combined_data, "combined_lha_2011_2018.csv")

# Save as R dataset formats
saveRDS(updated_combined_data, "combined_lha_2011_2018.rds")  # Single R object
save(updated_combined_data, file = "combined_lha_2011_2018.RData")  # R workspace format

# Summary of combined data
cat("\nTotal number of rows in combined dataset:", nrow(updated_combined_data), "\n")
cat("Years in combined dataset:", paste(unique(updated_combined_data$Year), collapse = ", "), "\n")
cat("Number of regions per year:\n")
updated_combined_data %>%
  group_by(Year) %>%
  summarise(Count = n_distinct(Region)) %>%
  as.data.frame() %>%
  print()


##### PART 2: Process 2019-2024 data #####

# First read the 2011-2018 combined data
combined_data_2018 <- read_csv("combined_lha_2011_2018.csv")

# Process 2019-2024 data
cat("Processing 2019 data...\n")
lha_2019 <- process_lha_excel_2019_2024("LHA_2019.xlsx", 2019, "Table 4")

cat("Processing 2020 data...\n")
lha_2020 <- process_lha_excel_2019_2024("LHA_2020.xlsx", 2020, "Table 4")

cat("Processing 2021 data...\n")
lha_2021 <- process_lha_excel_2019_2024("LHA_2021.xlsx", 2021, "Table 4")

cat("Processing 2022 data...\n")
lha_2022 <- process_lha_excel_2019_2024("LHA_2022.xlsx", 2022, "Table 4")

cat("Processing 2024 data...\n")
lha_2024 <- process_lha_excel_2019_2024("LHA_2024.xlsx", 2024, "Table 4")

# Combine all datasets
final_combined_data <- bind_rows(
  combined_data_2018,  # This already includes 2011-2018
  lha_2019,
  lha_2020,
  lha_2021,
  lha_2022,
  lha_2024
)

# Convert any string "NA" values to actual NA values
final_combined_data <- final_combined_data %>%
  mutate(across(where(is.character), ~ifelse(. == "NA", NA, .)))

# Ensure consistent data types across all columns
final_combined_data <- final_combined_data %>%
  mutate(
    Region = as.character(Region),
    Year = as.integer(Year),
    Shared = as.numeric(Shared),
    `1 Bedroom` = as.numeric(`1 Bedroom`),
    `2 Bedrooms` = as.numeric(`2 Bedrooms`),
    `3 Bedrooms` = as.numeric(`3 Bedrooms`),
    `4 Bedrooms` = as.numeric(`4 Bedrooms`),
    `5 Bedrooms` = as.numeric(`5 Bedrooms`)
  )

# Check region consistency for the full dataset
check_region_consistency(final_combined_data)

# Sort the final combined dataset by Region and Year
final_combined_data <- final_combined_data %>%
  arrange(Region, Year)

# Save the final combined dataset in multiple formats
write_csv(final_combined_data, "combined_lha_2011_2024.csv")

# Save as R dataset formats
saveRDS(final_combined_data, "combined_lha_2011_2024.rds")  # Single R object
save(final_combined_data, file = "combined_lha_2011_2024.RData")  # R workspace format

# Summary of final combined data
cat("\nTotal number of rows in final combined dataset:", nrow(final_combined_data), "\n")
cat("Years in final combined dataset:", paste(unique(final_combined_data$Year), collapse = ", "), "\n")
cat("Number of regions per year in final dataset:\n")
final_combined_data %>%
  group_by(Year) %>%
  summarise(Count = n_distinct(Region)) %>%
  as.data.frame() %>%
  print()