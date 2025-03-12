# Load required libraries
library(readr)
library(dplyr)

setwd("C:/Users/Gairui/OneDrive - University College London/Housing policy VRA/Processed")

# Read the existing combined data
combined_data <- read_csv("combined_lha_2011_2024.csv")

# Create 2023 data by duplicating 2022 data and changing the year
data_2022 <- combined_data %>% filter(Year == 2022)
data_2023 <- data_2022 %>% mutate(Year = 2023)

# Combine the new 2023 data with the existing dataset
updated_combined_data <- bind_rows(combined_data, data_2023)

# Sort the dataset by Region and Year
updated_combined_data <- updated_combined_data %>%
  arrange(Region, Year)

# Summary of updated combined data
cat("\nTotal number of rows in updated combined dataset:", nrow(updated_combined_data), "\n")
cat("Years in updated combined dataset:", paste(sort(unique(updated_combined_data$Year)), collapse = ", "), "\n")
cat("Number of regions per year in updated dataset:\n")
updated_combined_data %>%
  group_by(Year) %>%
  summarise(Count = n_distinct(Region)) %>%
  arrange(Year) %>%
  as.data.frame() %>%
  print()

# Save the updated combined dataset
write_csv(updated_combined_data, "combined_lha_2011_2024_updated.csv")

# Save as R dataset formats
saveRDS(updated_combined_data, "combined_lha_2011_2024_updated.rds")  # Single R object
save(updated_combined_data, file = "combined_lha_2011_2024_updated.RData")  # R workspace format

cat("\nUpdated dataset saved with 2023 data included.\n")
