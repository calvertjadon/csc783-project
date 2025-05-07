#############################################
# data_prep.R - US Accidents Data Preparation
# CSC 783 Data Visualization Project
#############################################

# ----------------------------
# 1. SETUP & CONFIGURATION
# ----------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(pryr)  # For memory monitoring
library(readr)  # For write_csv()

# Set working directory to project root
setwd("/Users/dominiquekellam/Desktop/__GRAD SPRING 2025/CSC 783 Data Visualization/Project")

# File paths
input_path <- "data/US_Accidents_March23.csv"  # Confirm exact filename
output_path <- "data/accidents_clean.rds"
log_path <- "logs/data_prep.log"

# Initialize logging
if (!dir.exists("logs")) dir.create("logs")
sink(log_path, append = FALSE, split = TRUE)
cat("==== Data Preparation Log ====\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ----------------------------
# 2. DATA IMPORT
# ----------------------------

import_accident_data <- function(file_path) {
  cat("=== Data Import ===\n")
  
  # Verify file exists
  if (!file.exists(file_path)) {
    stop("File not found at: ", file_path, 
         "\nFiles in data directory: ", paste(list.files("data/"), collapse=", "))
  }
  
  # Columns to keep (reduces memory usage)
  cols_to_keep <- c("ID", "Severity", "Start_Time", "End_Time", 
                    "State", "Weather_Condition", "City", "County",
                    "Temperature(F)", "Visibility(mi)", "Wind_Speed(mph)")
  
  # Memory monitoring
  cat("Memory before import:\n")
  print(pryr::mem_used())
  
  # Import with progress reporting
  cat("\nAttempting import...\n")
  tryCatch({
    dt <- fread(file_path, 
                select = cols_to_keep,
                showProgress = TRUE,
                verbose = TRUE)
    
    cat("\nImport successful. Dimensions:", dim(dt), "\n")
    cat("Sample data:\n")
    print(head(dt, 2))
    return(dt)
    
  }, error = function(e) {
    cat("\nImport failed. Error:", e$message, "\n")
    cat("Trying readr fallback...\n")
    
    library(readr)
    tryCatch({
      df <- read_csv(file_path, 
                     col_select = all_of(cols_to_keep),
                     progress = show_progress())
      return(df)
    }, error = function(e) {
      stop("All import methods failed:\n", e$message)
    })
  })
}

# Run import
accidents_raw <- import_accident_data(input_path)

# ----------------------------
# 3. DATA CLEANING
# ----------------------------

clean_accident_data <- function(raw_data) {
  cat("\n=== Data Cleaning ===\n")
  
  # Convert to tibble if needed
  if (data.table::is.data.table(raw_data)) {
    raw_data <- as_tibble(raw_data)
  }
  
  # Parse timestamps with detailed error handling
  cat("\nParsing timestamps...\n")
  cleaned <- raw_data %>%
    mutate(
      Start_Time_parsed = parse_date_time(Start_Time, 
                                          orders = c("ymd HMS", "mdy HMS", "ymd HM", "mdy HM"),
                                          quiet = TRUE),
      End_Time_parsed = parse_date_time(End_Time, 
                                        orders = c("ymd HMS", "mdy HMS", "ymd HM", "mdy HM"),
                                        quiet = TRUE),
      parse_failure = is.na(Start_Time_parsed) | is.na(End_Time_parsed)
    )
  
  # Report parse failures - CORRECTED BRACE PLACEMENT
  parse_fail_count <- sum(cleaned$parse_failure)
  if (parse_fail_count > 0) {
    cat("Warning:", parse_fail_count, 
        sprintf("(%.2f%%) rows failed timestamp parsing\n", 
                100*parse_fail_count/nrow(cleaned)))
    
    # Save failed rows
    failed_parses <- cleaned %>% filter(parse_failure)
    readr::write_csv(failed_parses, "logs/failed_time_parses.csv")
    cat("Saved failed rows to: logs/failed_time_parses.csv\n")
    
    cat("Sample of failed timestamps:\n")
    print(head(na.omit(failed_parses$Start_Time), 5))
  }  # <- Properly closed if-statement here
  
  # Process successful parses
  cleaned <- cleaned %>%
    filter(!parse_failure) %>%
    mutate(
      Start_Time = Start_Time_parsed,
      End_Time = End_Time_parsed,
      Hour = hour(Start_Time),
      Day = wday(Start_Time, label = TRUE, abbr = FALSE),
      Weekday = ifelse(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
      Month = month(Start_Time, label = TRUE, abbr = FALSE),
      Year = year(Start_Time),
      Duration = as.numeric(difftime(End_Time, Start_Time, units = "hours")),
      
      # Fixed column references
      Visibility_mi = ifelse(`Visibility(mi)` < 0, NA, `Visibility(mi)`),
      Wind_Speed_mph = ifelse(`Wind_Speed(mph)` < 0, NA, `Wind_Speed(mph)`)
    ) %>%
    select(-parse_failure, -Start_Time_parsed, -End_Time_parsed, 
           -`Visibility(mi)`, -`Wind_Speed(mph)`)
  
  return(cleaned)
}

# Run cleaning
accidents_clean <- clean_accident_data(accidents_raw)

# ----------------------------
# 4. DATA QUALITY CHECKS
# ----------------------------

check_data_quality <- function(data) {
  cat("\n=== Data Quality Report ===\n")
  
  # Basic dimensions
  cat("Dimensions:", dim(data), "\n")
  
  # Missing values
  missing_values <- sapply(data, function(x) sum(is.na(x)))
  cat("\nMissing Values:\n")
  print(missing_values[missing_values > 0])
  
  # Temporal sanity checks
  cat("\nTemporal Checks:\n")
  cat("- Date range:", 
      format(min(data$Start_Time, na.rm = TRUE), "%Y-%m-%d"), "to",
      format(max(data$Start_Time, na.rm = TRUE), "%Y-%m-%d"), "\n")
  
  duration_stats <- quantile(data$Duration, probs = c(0, 0.01, 0.5, 0.99, 1), na.rm = TRUE)
  cat("\nDuration Statistics (hours):\n")
  print(duration_stats)
  
  # Weather distribution
  cat("\nWeather Condition Distribution:\n")
  print(table(data$Weather_Condition))
  
  # Severity distribution
  cat("\nAccident Severity Distribution:\n")
  print(table(data$Severity))
}

check_data_quality(accidents_clean)

# ----------------------------
# 5. SAVE CLEANED DATA
# ----------------------------

cat("\n=== Saving Data ===\n")
saveRDS(accidents_clean, output_path)
cat("Saved cleaned data to:", output_path, "\n")

# Optional: Save sample for quick testing
set.seed(42)
accidents_sample <- accidents_clean %>% sample_n(10000)
saveRDS(accidents_sample, "data/accidents_sample.rds")

# ----------------------------
# 6. CLEANUP & FINAL REPORT
# ----------------------------

# Memory usage
cat("\nFinal Memory Usage:\n")
print(pryr::mem_used())

# Session info
cat("\n=== Session Info ===\n")
print(sessionInfo())

# Close log
cat("\nData preparation completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
sink()