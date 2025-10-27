# =============================================================================
# 01_data_preparation.R
# From Forecasts to Decisions: ED Staffing Analysis
# 
# This script loads and prepares the emergency department arrival data
# for forecasting and decision analysis.
# =============================================================================

# Load required libraries
library(tidyverse)
library(lubridate)
library(zoo)

# Set working directory to project root
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# Create output directories if they don't exist
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

# =============================================================================
# 1. Load Raw Data
# =============================================================================

# NOTE: Replace this with your actual data loading code
# Expected format: datetime, arrivals, and relevant covariates

# Example data loading:
# raw_data <- read_csv("data/raw/ed_arrivals.csv")

# For demonstration purposes, we'll create sample data structure
# Users should replace this with their actual data

cat("Loading ED arrival data...\n")

# Create sample data structure (replace with actual data)
# Uncomment and modify the following when you have actual data:
# raw_data <- read_csv("data/raw/ed_arrivals.csv") %>%
#   mutate(
#     datetime = ymd_hms(datetime),
#     arrivals = as.numeric(arrivals)
#   )

cat("Data loading complete. Please replace sample data structure with actual data.\n")

# =============================================================================
# 2. Data Cleaning
# =============================================================================

clean_ed_data <- function(data) {
  # Remove duplicates
  data <- data %>% distinct()
  
  # Handle missing values
  data <- data %>%
    arrange(datetime) %>%
    mutate(
      arrivals = ifelse(is.na(arrivals), 
                       zoo::na.approx(arrivals, na.rm = FALSE), 
                       arrivals)
    )
  
  # Remove outliers (beyond 3 standard deviations)
  data <- data %>%
    mutate(
      z_score = scale(arrivals)[,1],
      arrivals = ifelse(abs(z_score) > 3, NA, arrivals)
    ) %>%
    mutate(arrivals = zoo::na.approx(arrivals, na.rm = FALSE)) %>%
    select(-z_score)
  
  return(data)
}

# =============================================================================
# 3. Feature Engineering
# =============================================================================

engineer_features <- function(data) {
  data <- data %>%
    mutate(
      # Temporal features
      year = year(datetime),
      month = month(datetime),
      day = day(datetime),
      hour = hour(datetime),
      weekday = wday(datetime, label = TRUE),
      is_weekend = weekday %in% c("Sat", "Sun"),
      
      # Time-based features
      day_of_year = yday(datetime),
      week_of_year = week(datetime),
      
      # Cyclical encoding for hour
      hour_sin = sin(2 * pi * hour / 24),
      hour_cos = cos(2 * pi * hour / 24),
      
      # Cyclical encoding for day of week
      weekday_sin = sin(2 * pi * as.numeric(weekday) / 7),
      weekday_cos = cos(2 * pi * as.numeric(weekday) / 7),
      
      # Lagged features (previous hour, same hour yesterday, same hour last week)
      arrivals_lag1 = lag(arrivals, 1),
      arrivals_lag24 = lag(arrivals, 24),
      arrivals_lag168 = lag(arrivals, 168),
      
      # Rolling averages
      arrivals_ma3 = zoo::rollmean(arrivals, k = 3, fill = NA, align = "right"),
      arrivals_ma24 = zoo::rollmean(arrivals, k = 24, fill = NA, align = "right")
    )
  
  return(data)
}

# =============================================================================
# 4. Create Time Series Objects
# =============================================================================

create_ts_objects <- function(data) {
  # Create hourly time series
  ts_hourly <- ts(data$arrivals, frequency = 24)
  
  # Create daily aggregated time series
  daily_data <- data %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(date) %>%
    summarise(daily_arrivals = sum(arrivals, na.rm = TRUE))
  
  ts_daily <- ts(daily_data$daily_arrivals, frequency = 7)
  
  return(list(
    hourly = ts_hourly,
    daily = ts_daily,
    data = data,
    daily_data = daily_data
  ))
}

# =============================================================================
# 5. Train-Test Split
# =============================================================================

create_train_test_split <- function(data, train_prop = 0.8) {
  n <- nrow(data)
  train_size <- floor(n * train_prop)
  
  train_data <- data[1:train_size, ]
  test_data <- data[(train_size + 1):n, ]
  
  return(list(
    train = train_data,
    test = test_data,
    train_size = train_size,
    test_size = n - train_size
  ))
}

# =============================================================================
# 6. Main Execution
# =============================================================================

# Note: Uncomment and run when you have actual data
# 
# # Load data
# raw_data <- read_csv("data/raw/ed_arrivals.csv")
# 
# # Clean data
# clean_data <- clean_ed_data(raw_data)
# 
# # Engineer features
# processed_data <- engineer_features(clean_data)
# 
# # Create time series objects
# ts_objects <- create_ts_objects(processed_data)
# 
# # Create train-test split
# split_data <- create_train_test_split(processed_data)
# 
# # Save processed data
# write_csv(processed_data, "data/processed/ed_arrivals_processed.csv")
# saveRDS(ts_objects, "data/processed/ts_objects.rds")
# saveRDS(split_data, "data/processed/train_test_split.rds")
# 
# cat("Data preparation complete!\n")
# cat("Processed data saved to data/processed/\n")
# cat(sprintf("Training samples: %d\n", split_data$train_size))
# cat(sprintf("Testing samples: %d\n", split_data$test_size))

cat("\n=============================================================================\n")
cat("Data Preparation Script Ready\n")
cat("Please add your ED arrival data to data/raw/ and uncomment the execution code\n")
cat("=============================================================================\n")
