#################################################################
# SCRIPT TO CLEAN RENT DATA & JOIN ALL 2009-2016 HOMELESSNESS DATA
# Version with specified directory paths
#################################################################

# --- 1. Load Necessary Libraries ---
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)
library(purrr)

# --- 2. Set Data Directory Paths ---
# Set path for homelessness data
homelessness_data_dir <- "C:/Users/Gairui/OneDrive - University College London/Housing policy VRA/Datasets/Detailed_local_authority_level_homelessness_figures_2009_to_2016"

# Set rent data file path (adjust as needed)
# Option 1: If rent file is within homelessness directory
rent_file_path <- file.path(homelessness_data_dir, "PRPbyLA2015_2024.xlsx")

# --- 3. Validate File Paths ---
cat("=== CHECKING FILE PATHS ===\n")
cat("Homelessness directory:", homelessness_data_dir, "\n")
cat("Directory exists:", dir.exists(homelessness_data_dir), "\n")

if (!dir.exists(homelessness_data_dir)) {
  stop("Homelessness data directory does not exist. Please check the path.")
}

cat("Rent file path:", rent_file_path, "\n")
cat("Rent file exists:", file.exists(rent_file_path), "\n")

if (!file.exists(rent_file_path)) {
  cat("Rent file not found. Please check one of these locations:\n")
  cat("1. In homelessness directory:", file.path(homelessness_data_dir, "PRPbyLA2015_2024.xlsx"), "\n")
  cat("2. In parent directory:", file.path(dirname(homelessness_data_dir), "PRPbyLA2015_2024.xlsx"), "\n")
  cat("3. In current working directory:", file.path(getwd(), "PRPbyLA2015_2024.xlsx"), "\n")
  stop("Please update rent_file_path with the correct location.")
}

# --- 4. Create the Clean Rent Price Dataset ---
rent_prices <- read_excel(rent_file_path, skip = 7)

rent_prices_clean <- rent_prices %>%
  rename(
    date = `Time period`,
    area_code = `Area code`,
    area_name = `Area name`,
    region = `Region or country name`,
    annual_change_pct = `Annual change (%)`,
    rental_price_gbp = `Rental price (£)`
  ) %>%
  mutate(
    rental_price_gbp = suppressWarnings(as.numeric(rental_price_gbp)),
    annual_change_pct = suppressWarnings(as.numeric(annual_change_pct)),
    date = as.Date(date)
  )

cat("\n=== RENT DATA LOADED ===\n")
cat("Rent data dimensions:", dim(rent_prices_clean), "\n")
cat("Date range:", paste(range(rent_prices_clean$date, na.rm = TRUE), collapse = " to "), "\n")

# --- 5. Enhanced Function to Clean Homelessness Files ---
clean_homelessness_file <- function(file_path, year_val, quarter_val) {
  cat("\n--- Processing:", basename(file_path), "---\n")
  
  tryCatch({
    # Check sheet names
    sheets <- excel_sheets(file_path)
    sheet_name <- NULL
    
    # Find correct sheet name
    possible_names <- c("Section 6", "Section6", "Sec 6", "Sec6", "Sheet1")
    for (name in possible_names) {
      if (name %in% sheets) {
        sheet_name <- name
        break
      }
    }
    
    if (is.null(sheet_name)) {
      cat("Available sheets:", paste(sheets, collapse = ", "), "\n")
      cat("Using first sheet:", sheets[1], "\n")
      sheet_name <- sheets[1]
    }
    
    # Try various header and data start configurations
    for (header_row in 5:8) {
      for (data_start in (header_row + 1):(header_row + 3)) {
        tryCatch({
          # Read headers
          headers <- read_excel(file_path, sheet = sheet_name, 
                                skip = header_row, n_max = 1, col_names = FALSE) %>%
            unlist() %>% as.character()
          
          # Read data
          data <- read_excel(file_path, sheet = sheet_name, 
                             skip = data_start, col_names = headers, col_types = "text")
          
          if (ncol(data) >= 3 && nrow(data) > 0) {
            # Ensure first two columns are area_code and area_name
            names(data)[1:2] <- c("area_code", "area_name")
            
            # Clean and transform data
            processed <- data %>%
              filter(!is.na(area_code), 
                     area_code != "", 
                     !str_detect(str_to_upper(area_code), "TOTAL|ENGLAND|WALES|SCOTLAND")) %>%
              pivot_longer(cols = -c(area_code, area_name),
                           names_to = "homelessness_code", 
                           values_to = "households") %>%
              mutate(
                households = suppressWarnings(as.numeric(households)),
                year = year_val,
                quarter = quarter_val,
                area_code = str_squish(as.character(area_code)),
                area_name = str_squish(as.character(area_name))
              ) %>%
              filter(!is.na(households), households != 0) %>%
              select(year, quarter, area_code, area_name, homelessness_code, households)
            
            if (nrow(processed) > 10) {  # If reasonable number of records
              cat("Success! Rows:", nrow(processed), "Cols:", ncol(data) - 2, "\n")
              return(processed)
            }
          }
        }, error = function(e) NULL)
      }
    }
    
    cat("Failed to process file\n")
    return(NULL)
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# --- 6. Find and Process Homelessness Files ---
# Find files
all_files <- list.files(homelessness_data_dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)

cat("\n=== SEARCHING FOR HOMELESSNESS FILES ===\n")
cat("Total Excel files in directory:", length(all_files), "\n")

# Filter for quarterly files
quarterly_files <- all_files[str_detect(basename(all_files), 
                                        "(?i)(January|April|July|October|jan|apr|jul|oct)")]

cat("Quarterly files found:", length(quarterly_files), "\n")
for (file in quarterly_files) {
  cat("-", basename(file), "\n")
}

if (length(quarterly_files) == 0) {
  cat("No quarterly files found. All files in directory:\n")
  for (file in all_files) {
    cat("-", basename(file), "\n")
  }
  stop("Please check file naming pattern")
}

# Create processing list
files_to_process <- tibble(file_path = quarterly_files) %>%
  mutate(
    filename = basename(file_path),
    year = as.numeric(str_extract(filename, "\\d{4}")),
    quarter_month = str_to_lower(str_extract(filename, "(?i)(January|April|July|October|jan|apr|jul|oct)")),
    quarter = case_when(
      str_detect(quarter_month, "jan") ~ "Q1",
      str_detect(quarter_month, "apr") ~ "Q2",
      str_detect(quarter_month, "jul") ~ "Q3",
      str_detect(quarter_month, "oct") ~ "Q4",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(year), !is.na(quarter))

cat("\n=== FILES TO PROCESS ===\n")
print(files_to_process)

# Process all files
homelessness_list <- pmap(
  list(files_to_process$file_path, files_to_process$year, files_to_process$quarter),
  clean_homelessness_file
)

# Remove failed attempts
homelessness_list <- homelessness_list[!sapply(homelessness_list, is.null)]
homelessness_all_long <- bind_rows(homelessness_list)

cat("\n=== HOMELESSNESS DATA SUMMARY ===\n")
cat("Total records:", nrow(homelessness_all_long), "\n")
cat("Years:", paste(sort(unique(homelessness_all_long$year)), collapse = ", "), "\n")
cat("Quarters:", paste(sort(unique(homelessness_all_long$quarter)), collapse = ", "), "\n")
cat("Unique area codes:", length(unique(homelessness_all_long$area_code)), "\n")

# --- 7. Join the Datasets ---
# Prepare rent data for joining
rent_prices_for_join <- rent_prices_clean %>%
  mutate(
    year = year(date),
    quarter = paste0("Q", quarter(date)),
    area_code = str_squish(as.character(area_code)),
    area_name = str_squish(as.character(area_name))
  )

# Perform join
final_combined_data <- left_join(
  rent_prices_for_join,
  homelessness_all_long,
  by = c("area_code", "area_name", "year", "quarter")
)

# Check join results
cat("\n=== JOIN RESULTS ===\n")
cat("Final dataset dimensions:", dim(final_combined_data), "\n")
cat("Records with homelessness data:", sum(!is.na(final_combined_data$households)), "\n")

# Display successful join examples
success_examples <- final_combined_data %>%
  filter(!is.na(households)) %>%
  head(5)

if (nrow(success_examples) > 0) {
  cat("Sample successful joins:\n")
  print(success_examples %>% select(date, area_code, area_name, homelessness_code, households))
} else {
  cat("WARNING: No records successfully joined!\n")
  
  # Diagnose join issues
  rent_2015_2016 <- rent_prices_for_join %>% 
    filter(year %in% 2015:2016) %>% 
    select(area_code, area_name, year, quarter) %>% 
    distinct()
  
  homeless_2015_2016 <- homelessness_all_long %>% 
    filter(year %in% 2015:2016) %>% 
    select(area_code, area_name, year, quarter) %>% 
    distinct()
  
  cat("Rent keys (2015-2016):", nrow(rent_2015_2016), "\n")
  cat("Homeless keys (2015-2016):", nrow(homeless_2015_2016), "\n")
  
  # Check area code matching
  common_codes <- intersect(rent_2015_2016$area_code, homeless_2015_2016$area_code)
  cat("Common area codes:", length(common_codes), "\n")
  
  if (length(common_codes) > 0) {
    cat("Sample common codes:", head(common_codes, 5), "\n")
  }
}

# --- 8. Save Results ---
output_dir <- dirname(rent_file_path)  # Save to rent file directory
rds_file <- file.path(output_dir, "rent_and_homelessness_2009-2016.rds")
csv_file <- file.path(output_dir, "rent_and_homelessness_2009-2016.csv")

saveRDS(final_combined_data, file = rds_file)
write.csv(final_combined_data, file = csv_file, row.names = FALSE)

cat("\n=== FILES SAVED ===\n")
cat("RDS file:", rds_file, "\n")
cat("CSV file:", csv_file, "\n")
cat("Script completed successfully!\n")