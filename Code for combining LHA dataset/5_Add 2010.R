# Load required libraries
library(readr)
library(dplyr)

# Set working directory
# Replace this with your actual working directory if running on your machine
setwd("C:/Users/Gairui/OneDrive - University College London/Housing policy VRA/Processed")

# Read the existing combined data (2011-2024)
combined_data <- read_csv("combined_lha_2011_2024_updated.csv")

# Read the 2010 LHA data
lha_2010 <- read_csv("LHA_2010_UTF8.csv")

# Print the structure of the 2010 data to understand column names
cat("Structure of 2010 LHA data:\n")
str(lha_2010)

# Check for any duplicate regions
cat("\nTotal number of unique regions in 2010 data:", length(unique(lha_2010$BRMA)), "\n")

# Process the 2010 data to match the structure of the combined dataset
lha_2010_processed <- lha_2010 %>%
  # Rename columns to match the combined dataset
  rename(
    Region = BRMA,
    Shared = Shared,
    `1 Bedroom` = `1 Bedroom`,
    `2 Bedrooms` = `2 Bedrooms`,
    `3 Bedrooms` = `3 Bedrooms`,
    `4 Bedrooms` = `4 Bedrooms`,
    `5 Bedrooms` = `5 bedrooms`  # Note the capitalization difference
  ) %>%
  # Add Year column
  mutate(Year = 2010) %>%
  # Select only the columns needed for the combined dataset
  select(Region, Year, Shared, `1 Bedroom`, `2 Bedrooms`, `3 Bedrooms`, `4 Bedrooms`, `5 Bedrooms`)

# Check if there are any NA values in the Region column
cat("\nNumber of NA values in Region column:", sum(is.na(lha_2010_processed$Region)), "\n")

# Print sample of processed 2010 data
cat("\nSample of processed 2010 data:\n")
print(head(lha_2010_processed, 3))

# Check for regions in 2010 data that are not in the combined dataset
existing_regions <- unique(combined_data$Region)
new_regions_2010 <- setdiff(unique(lha_2010_processed$Region), existing_regions)

cat("\nRegions in 2010 data that are not in the combined dataset:\n")
if (length(new_regions_2010) > 0) {
  print(new_regions_2010)
} else {
  cat("None\n")
}

# Function to check region consistency as in the original script
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

# Combine the processed 2010 data with the existing combined dataset
final_combined_data <- bind_rows(
  lha_2010_processed,
  combined_data
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

# Sort the final combined dataset by Region and Year (chronological order)
final_combined_data <- final_combined_data %>%
  arrange(Region, Year)

# Check region consistency for the full dataset
cat("\nChecking region consistency for the combined 2010-2024 dataset:\n")
check_region_consistency(final_combined_data)

# Save the final combined dataset in multiple formats
write_csv(final_combined_data, "combined_lha_2010_2024.csv")

# Save as R dataset formats
saveRDS(final_combined_data, "combined_lha_2010_2024.rds")  # Single R object
save(final_combined_data, file = "combined_lha_2010_2024.RData")  # R workspace format

# Summary of final combined data
cat("\nTotal number of rows in final combined dataset:", nrow(final_combined_data), "\n")
cat("Years in final combined dataset:", paste(unique(final_combined_data$Year), collapse = ", "), "\n")
cat("Number of regions per year in final dataset:\n")
final_combined_data %>%
  group_by(Year) %>%
  summarise(Count = n_distinct(Region)) %>%
  arrange(Year) %>%  # Ensure years are in chronological order
  as.data.frame() %>%
  print()
