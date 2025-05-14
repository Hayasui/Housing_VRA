#Load packages
library(tidyverse)
library(readr)
library(labelled)

# Set working dictionary
setwd("C:/Users/Gairui/OneDrive - University College London/Housing policy VRA/Datasets/LSOAtoBRMA")

# Step 1: Load the datasets
# Loading NSPL with specific columns we need
nspl_data <- read_csv("NSPL_AUG_2021_UK.csv", 
                      col_types = cols_only(
                        pcds = col_character(),    # Postcode (standard format)
                        lsoa11 = col_character()   # LSOA code
                      ))

# Load the BRMA lookup dataset
brma_data <- read_csv("pcode_brma_lookup_v2.2.csv")

glimpse(nspl_data)
glimpse(brma_data)

# Step 2: Data cleaning and preparation
# Clean the NSPL data - remove NAs
nspl_data <- nspl_data %>%
  filter(!is.na(lsoa11) & !is.na(pcds))

# Clean the BRMA data - adjust column names to match
brma_data <- brma_data %>%
  # Convert column names to lowercase for consistency
  rename_with(tolower) %>%
  # Filter out any NA values in key fields
  filter(!is.na(pcds) & !is.na(brma) & !is.na(brma_name)) %>%
  # Select only the columns we need
  select(pcds, brma, brma_name)

# Step 3: Join the datasets
# Join NSPL data with BRMA data using the postcode field
merged_data <- nspl_data %>%
  inner_join(brma_data, by = "pcds")

# Step 4: Process to ensure LSOA to BRMA mapping
# Create a dataset with LSOA and corresponding BRMA information
lsoa_brma_mapping <- merged_data %>%
  # Get distinct combinations of LSOA and BRMA
  distinct(lsoa11, pcds, brma, brma_name)

# Step 5: Final output formatting
# Final output with LSOA-BRMA mapping
final_output <- lsoa_brma_mapping %>%
  rename(pcd = pcds) %>%
  arrange(lsoa11)

# Add variable labels to the dataset
final_output <- final_output %>%
  set_variable_labels(
    lsoa11 = "2011 Census Lower Layer Super Output Area Code",
    pcd = "Postcode (Standard Format)",
    brma = "Broad Rental Market Area Code",
    brma_name = "Broad Rental Market Area Name"
  )

View(final_output)

# Step 6: Save the results
# Save the results in csv format
write_csv(final_output, "LSOA_to_BRMA_Mapping.csv")

# Save the results in RDS format
saveRDS(final_output, "LSOA_to_BRMA_Mapping.rds")

# To verify that labels have been correctly added
str(final_output)
var_label(final_output)

# Additional analysis: Check for LSOAs with multiple BRMAs
lsoa_brma_counts <- final_output %>%
  group_by(lsoa11) %>%
  summarize(
    brma_count = n_distinct(brma),
    postcodes = n_distinct(pcd)
  )

# View summary of LSOAs with multiple BRMAs
multi_brma_lsoas <- lsoa_brma_counts %>%
  filter(brma_count > 1)

# Print summary statistics
cat("Total unique LSOAs:", nrow(lsoa_brma_counts), "\n")
cat("LSOAs with multiple BRMAs:", nrow(multi_brma_lsoas), "\n")
