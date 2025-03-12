# Load required libraries
library(readxl)
library(dplyr)
library(readr)

# Set working directory to your path
setwd("C:/Users/Gairui/OneDrive - University College London/Housing policy VRA/Processed")

# Read existing combined data (2011-2014)
combined_data <- read_csv("combined_lha_2011_2014.csv")

# Function to read and process LHA data from Excel files
process_lha_excel <- function(file_path, year, sheet_name) {
  # Read Excel data - skip the title row (starting from row 2 which contains column names)
  # Set .name_repair to "minimal" to prevent excessive renaming
  lha_data <- read_excel(file_path, sheet = sheet_name, skip = 1, .name_repair = "minimal")
  
  # Print column names to debug
  cat("Original column names in", file_path, ":", paste(colnames(lha_data), collapse = ", "), "\n")
  
  # Extract only the first 6 columns (BRMA and the 5 weekly rates)
  lha_data <- lha_data[, 1:6]
  
  # Now rename the columns manually
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
  
  # Add 5 Bedrooms column with NA values to match the combined dataset
  lha_data$`5 Bedrooms` <- NA
  
  # Reorder columns to match the combined dataset
  lha_data <- lha_data %>%
    select(Region, Year, Shared, `1 Bedroom`, `2 Bedrooms`, `3 Bedrooms`, `4 Bedrooms`, `5 Bedrooms`)
  
  return(lha_data)
}

# Try with a simplified approach
process_lha_excel_alt <- function(file_path, year, sheet_name) {
  # Read Excel data directly with col_names=FALSE to ignore header names
  lha_data <- read_excel(file_path, sheet = sheet_name, skip = 2, col_names = FALSE)
  
  # Print the dimensions to verify data
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
  
  # Reorder columns
  lha_data <- lha_data %>%
    select(Region, Year, Shared, `1 Bedroom`, `2 Bedrooms`, `3 Bedrooms`, `4 Bedrooms`, `5 Bedrooms`)
  
  return(lha_data)
}

# Try the alternative approach instead
cat("Processing 2015 data with alternative approach...\n")
lha_2015 <- process_lha_excel_alt("LHA_2015.xls", 2015, "Table 4")

cat("Processing 2016 data with alternative approach...\n")
lha_2016 <- process_lha_excel_alt("LHA_2016.xls", 2016, "Table 3")

# Print head of data to verify
cat("\nFirst few rows of 2015 data:\n")
print(head(lha_2015, 3))

cat("\nFirst few rows of 2016 data:\n")
print(head(lha_2016, 3))

# Combine all datasets
final_combined_data <- bind_rows(combined_data, lha_2015, lha_2016)

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

# Make sure region names are consistent
# This function can help identify any inconsistencies in region names
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

# Check region consistency
check_region_consistency(final_combined_data)

# Sort the final combined dataset by Region and Year
final_combined_data <- final_combined_data %>%
  arrange(Region, Year)

# Save the final combined dataset
write_csv(final_combined_data, "combined_lha_2011_2016.csv")

# Summary of combined data
cat("\nTotal number of rows in combined dataset:", nrow(final_combined_data), "\n")
cat("Years in combined dataset:", paste(unique(final_combined_data$Year), collapse = ", "), "\n")
cat("Number of regions per year:\n")
final_combined_data %>%
  group_by(Year) %>%
  summarise(Count = n_distinct(Region)) %>%
  as.data.frame() %>%
  print()
