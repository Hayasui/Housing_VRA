# Load necessary libraries
library(readr)      # For reading CSV files
library(readxl)     # For reading Excel files
library(dplyr)      # For data manipulation
library(stringr)    # For string manipulation

# Set working directory to your location
setwd("C:/Users/Gairui/OneDrive - University College London/Housing policy VRA/Processed")

# STEP 1: Read and examine the files
cat("Reading 2011, 2012, 2013, and 2014 datasets...\n")

# Read 2011 data (CSV)
data_2011 <- read_csv("LHA_2011.csv")
cat("2011 columns:", paste(names(data_2011), collapse=", "), "\n")

# Read 2012 data (CSV)
data_2012 <- read_csv("LHA_2012.csv")
cat("2012 columns:", paste(names(data_2012), collapse=", "), "\n")

# Read 2013 data (XLS)
data_2013 <- read_excel("LHA_2013.xls", sheet = 1)
cat("2013 columns:", paste(names(data_2013), collapse=", "), "\n")

# Read 2014 data (XLS)
data_2014 <- read_excel("LHA_2014.xls", sheet = 1)
cat("2014 columns:", paste(names(data_2014), collapse=", "), "\n")

# STEP 2: Process 2011 data
cat("\nProcessing 2011 data...\n")
data_2011_clean <- data_2011 %>%
  # Add year column
  mutate(Year = 2011) %>%
  # Convert string bedroom columns to numeric if needed
  mutate(across(c(`4 Bedrooms`, `5 Bedrooms`), 
                ~if(is.character(.)) as.numeric(str_replace_all(., "[^0-9.]", "")) else .)) %>%
  # Select and rename columns as required
  select(
    Region = BRMA,
    Year,
    Shared,
    `1 Bedroom`,
    `2 Bedrooms`,
    `3 Bedrooms`,
    `4 Bedrooms`,
    `5 Bedrooms`
  )

# STEP 3: Process 2012 data
cat("Processing 2012 data...\n")
data_2012_clean <- data_2012 %>%
  # Add year column
  mutate(Year = 2012) %>%
  # Select and rename columns
  select(
    Region = BRMA,
    Year,
    Shared,
    `1 Bedroom`,
    `2 Bedrooms`,
    `3 Bedrooms`,
    `4 Bedrooms`
  ) %>%
  # Add missing 5 Bedrooms column with NA values
  mutate(`5 Bedrooms` = NA)

# STEP 4: Process 2013 data
cat("Processing 2013 data...\n")
data_2013_clean <- data_2013 %>%
  # Add year column
  mutate(Year = 2013) %>%
  # Find and rename bedroom columns - column names might be different in XLS file
  rename_with(
    ~ case_when(
      grepl("BRMA|Area", ., ignore.case = TRUE) ~ "Region",
      grepl("shared", ., ignore.case = TRUE) ~ "Shared",
      grepl("1 bed|1bed", ., ignore.case = TRUE) ~ "1 Bedroom",
      grepl("2 bed|2bed", ., ignore.case = TRUE) ~ "2 Bedrooms",
      grepl("3 bed|3bed", ., ignore.case = TRUE) ~ "3 Bedrooms",
      grepl("4 bed|4bed", ., ignore.case = TRUE) ~ "4 Bedrooms",
      TRUE ~ .
    )
  ) %>%
  # Select required columns
  select(Region, Year, Shared, `1 Bedroom`, `2 Bedrooms`, `3 Bedrooms`, `4 Bedrooms`) %>%
  # Add missing 5 Bedrooms column with NA values
  mutate(`5 Bedrooms` = NA)

# STEP 5: Process 2014 data
cat("Processing 2014 data...\n")
data_2014_clean <- data_2014 %>%
  # Add year column
  mutate(Year = 2014) %>%
  # Find and rename bedroom columns - column names might be different in XLS file
  rename_with(
    ~ case_when(
      grepl("BRMA|Area", ., ignore.case = TRUE) ~ "Region",
      grepl("shared", ., ignore.case = TRUE) ~ "Shared",
      grepl("1 bed|1bed", ., ignore.case = TRUE) ~ "1 Bedroom",
      grepl("2 bed|2bed", ., ignore.case = TRUE) ~ "2 Bedrooms",
      grepl("3 bed|3bed", ., ignore.case = TRUE) ~ "3 Bedrooms",
      grepl("4 bed|4bed", ., ignore.case = TRUE) ~ "4 Bedrooms",
      TRUE ~ .
    )
  ) %>%
  # Select required columns
  select(Region, Year, Shared, `1 Bedroom`, `2 Bedrooms`, `3 Bedrooms`, `4 Bedrooms`) %>%
  # Add missing 5 Bedrooms column with NA values
  mutate(`5 Bedrooms` = NA)

# STEP 6: Combine datasets
cat("Combining datasets...\n")
combined_data <- bind_rows(
  data_2011_clean, 
  data_2012_clean,
  data_2013_clean,
  data_2014_clean
) %>%
  # Arrange in chronological order by region and year
  arrange(Region, Year)

# STEP 7: Check the combined dataset
cat("\nCombined dataset summary:\n")
cat("Number of rows:", nrow(combined_data), "\n")
cat("Number of columns:", ncol(combined_data), "\n")
cat("Column names:", paste(names(combined_data), collapse=", "), "\n")

# Display a few rows to verify
cat("\nSample rows from combined dataset:\n")
print(head(combined_data, 3))

# STEP 8: Save the combined dataset
output_file <- "combined_lha_2011_2014.csv"
write_csv(combined_data, output_file)
cat("\nCombined data saved to:", output_file, "\n")