# =============================================================================
# 02_exploratory_analysis.R
# From Forecasts to Decisions: ED Staffing Analysis
# 
# This script performs exploratory data analysis on ED arrival data
# =============================================================================

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)

# Set working directory to project root
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd("..")
} else {
  # If not in RStudio, assume we're already in project root
  cat("Note: Not running in RStudio. Ensure working directory is set to project root.\n")
}

# Create output directories
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

# =============================================================================
# 1. Load Processed Data
# =============================================================================

# Load processed data from previous step
# processed_data <- read_csv("data/processed/ed_arrivals_processed.csv")
# ts_objects <- readRDS("data/processed/ts_objects.rds")

# =============================================================================
# 2. Descriptive Statistics
# =============================================================================

calculate_descriptive_stats <- function(data) {
  stats <- data %>%
    summarise(
      n_observations = n(),
      mean_arrivals = mean(arrivals, na.rm = TRUE),
      median_arrivals = median(arrivals, na.rm = TRUE),
      sd_arrivals = sd(arrivals, na.rm = TRUE),
      min_arrivals = min(arrivals, na.rm = TRUE),
      max_arrivals = max(arrivals, na.rm = TRUE),
      q25_arrivals = quantile(arrivals, 0.25, na.rm = TRUE),
      q75_arrivals = quantile(arrivals, 0.75, na.rm = TRUE)
    )
  
  # By hour of day
  hourly_stats <- data %>%
    group_by(hour) %>%
    summarise(
      mean_arrivals = mean(arrivals, na.rm = TRUE),
      sd_arrivals = sd(arrivals, na.rm = TRUE),
      .groups = "drop"
    )
  
  # By day of week
  daily_stats <- data %>%
    group_by(weekday) %>%
    summarise(
      mean_arrivals = mean(arrivals, na.rm = TRUE),
      sd_arrivals = sd(arrivals, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    overall = stats,
    hourly = hourly_stats,
    daily = daily_stats
  ))
}

# =============================================================================
# 3. Time Series Plots
# =============================================================================

plot_time_series <- function(data) {
  # Overall time series
  p1 <- ggplot(data, aes(x = datetime, y = arrivals)) +
    geom_line(color = "steelblue", alpha = 0.7) +
    theme_minimal() +
    labs(
      title = "ED Arrivals Over Time",
      x = "Date/Time",
      y = "Number of Arrivals"
    )
  
  # Daily aggregated pattern
  daily_agg <- data %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(date) %>%
    summarise(daily_arrivals = sum(arrivals, na.rm = TRUE), .groups = "drop")
  
  p2 <- ggplot(daily_agg, aes(x = date, y = daily_arrivals)) +
    geom_line(color = "darkgreen", alpha = 0.7) +
    theme_minimal() +
    labs(
      title = "Daily ED Arrivals",
      x = "Date",
      y = "Total Daily Arrivals"
    )
  
  return(list(hourly_plot = p1, daily_plot = p2))
}

# =============================================================================
# 4. Pattern Analysis
# =============================================================================

plot_patterns <- function(data) {
  # Hourly pattern
  p1 <- data %>%
    group_by(hour) %>%
    summarise(
      mean_arrivals = mean(arrivals, na.rm = TRUE),
      sd_arrivals = sd(arrivals, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = hour, y = mean_arrivals)) +
    geom_line(color = "steelblue", size = 1) +
    geom_ribbon(aes(ymin = mean_arrivals - sd_arrivals, 
                    ymax = mean_arrivals + sd_arrivals),
                alpha = 0.2, fill = "steelblue") +
    scale_x_continuous(breaks = seq(0, 23, 2)) +
    theme_minimal() +
    labs(
      title = "Average Hourly Pattern",
      x = "Hour of Day",
      y = "Mean Arrivals ± SD"
    )
  
  # Day of week pattern
  p2 <- data %>%
    group_by(weekday) %>%
    summarise(
      mean_arrivals = mean(arrivals, na.rm = TRUE),
      sd_arrivals = sd(arrivals, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = weekday, y = mean_arrivals)) +
    geom_col(fill = "darkgreen", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_arrivals - sd_arrivals,
                      ymax = mean_arrivals + sd_arrivals),
                  width = 0.2) +
    theme_minimal() +
    labs(
      title = "Average Pattern by Day of Week",
      x = "Day of Week",
      y = "Mean Arrivals ± SD"
    )
  
  # Heatmap: hour x weekday
  p3 <- data %>%
    group_by(hour, weekday) %>%
    summarise(mean_arrivals = mean(arrivals, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = hour, y = weekday, fill = mean_arrivals)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkred") +
    scale_x_continuous(breaks = seq(0, 23, 2)) +
    theme_minimal() +
    labs(
      title = "Arrival Patterns: Hour x Day of Week",
      x = "Hour of Day",
      y = "Day of Week",
      fill = "Mean\nArrivals"
    )
  
  return(list(
    hourly_pattern = p1,
    weekday_pattern = p2,
    heatmap = p3
  ))
}

# =============================================================================
# 5. Seasonality Analysis
# =============================================================================

analyze_seasonality <- function(data) {
  # Monthly pattern
  monthly <- data %>%
    group_by(year, month) %>%
    summarise(
      mean_arrivals = mean(arrivals, na.rm = TRUE),
      total_arrivals = sum(arrivals, na.rm = TRUE),
      .groups = "drop"
    )
  
  p1 <- ggplot(monthly, aes(x = month, y = mean_arrivals, group = year, color = factor(year))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 1:12, 
                      labels = month.abb) +
    theme_minimal() +
    labs(
      title = "Monthly Patterns by Year",
      x = "Month",
      y = "Mean Hourly Arrivals",
      color = "Year"
    )
  
  return(list(
    monthly_data = monthly,
    monthly_plot = p1
  ))
}

# =============================================================================
# 6. Distribution Analysis
# =============================================================================

plot_distributions <- function(data) {
  # Overall distribution
  p1 <- ggplot(data, aes(x = arrivals)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    theme_minimal() +
    labs(
      title = "Distribution of ED Arrivals",
      x = "Number of Arrivals",
      y = "Frequency"
    )
  
  # Box plot by hour
  p2 <- ggplot(data, aes(x = factor(hour), y = arrivals)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    labs(
      title = "Arrival Distribution by Hour",
      x = "Hour of Day",
      y = "Number of Arrivals"
    )
  
  # Box plot by weekday
  p3 <- ggplot(data, aes(x = weekday, y = arrivals)) +
    geom_boxplot(fill = "lightgreen", alpha = 0.7) +
    theme_minimal() +
    labs(
      title = "Arrival Distribution by Weekday",
      x = "Day of Week",
      y = "Number of Arrivals"
    )
  
  return(list(
    histogram = p1,
    boxplot_hour = p2,
    boxplot_weekday = p3
  ))
}

# =============================================================================
# 7. Main Execution
# =============================================================================

# Note: Uncomment and run when you have processed data
# 
# # Load data
# processed_data <- read_csv("data/processed/ed_arrivals_processed.csv") %>%
#   mutate(datetime = ymd_hms(datetime))
# 
# # Calculate descriptive statistics
# desc_stats <- calculate_descriptive_stats(processed_data)
# write_csv(desc_stats$overall, "results/descriptive_statistics_overall.csv")
# write_csv(desc_stats$hourly, "results/descriptive_statistics_hourly.csv")
# write_csv(desc_stats$daily, "results/descriptive_statistics_daily.csv")
# 
# # Generate time series plots
# ts_plots <- plot_time_series(processed_data)
# ggsave("figures/01_time_series_hourly.png", ts_plots$hourly_plot, width = 12, height = 6)
# ggsave("figures/02_time_series_daily.png", ts_plots$daily_plot, width = 12, height = 6)
# 
# # Generate pattern plots
# pattern_plots <- plot_patterns(processed_data)
# ggsave("figures/03_hourly_pattern.png", pattern_plots$hourly_pattern, width = 10, height = 6)
# ggsave("figures/04_weekday_pattern.png", pattern_plots$weekday_pattern, width = 10, height = 6)
# ggsave("figures/05_heatmap_hour_weekday.png", pattern_plots$heatmap, width = 10, height = 6)
# 
# # Analyze seasonality
# seasonality <- analyze_seasonality(processed_data)
# write_csv(seasonality$monthly_data, "results/monthly_patterns.csv")
# ggsave("figures/06_monthly_patterns.png", seasonality$monthly_plot, width = 10, height = 6)
# 
# # Plot distributions
# dist_plots <- plot_distributions(processed_data)
# ggsave("figures/07_distribution_histogram.png", dist_plots$histogram, width = 10, height = 6)
# ggsave("figures/08_distribution_by_hour.png", dist_plots$boxplot_hour, width = 12, height = 6)
# ggsave("figures/09_distribution_by_weekday.png", dist_plots$boxplot_weekday, width = 10, height = 6)
# 
# cat("\n=============================================================================\n")
# cat("Exploratory Analysis Complete!\n")
# cat("Figures saved to figures/\n")
# cat("Results saved to results/\n")
# cat("=============================================================================\n")

cat("\n=============================================================================\n")
cat("Exploratory Analysis Script Ready\n")
cat("Please run 01_data_preparation.R first, then uncomment the execution code\n")
cat("=============================================================================\n")
