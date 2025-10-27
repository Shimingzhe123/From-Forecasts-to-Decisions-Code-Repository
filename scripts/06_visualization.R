# =============================================================================
# 06_visualization.R
# From Forecasts to Decisions: ED Staffing Analysis
# 
# This script creates publication-quality visualizations
# =============================================================================

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)
library(lubridate)

# Set working directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd("..")
} else {
  # If not in RStudio, assume we're already in project root
  cat("Note: Not running in RStudio. Ensure working directory is set to project root.\n")
}

# Create output directories
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

# Set theme for publication-quality plots
theme_publication <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

theme_set(theme_publication)

# =============================================================================
# 1. Forecast Comparison Plots
# =============================================================================

plot_forecast_comparison <- function(test_data, forecasts_list, zoom_range = 1:168) {
  # Prepare data
  plot_data <- data.frame(
    time_index = seq_along(test_data$arrivals),
    actual = test_data$arrivals
  )
  
  # Add forecasts from each model
  for (model_name in names(forecasts_list)) {
    plot_data[[model_name]] <- forecasts_list[[model_name]]
  }
  
  # Reshape for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(cols = -time_index, names_to = "series", values_to = "value")
  
  # Full comparison
  p1 <- ggplot(plot_data_long, aes(x = time_index, y = value, color = series)) +
    geom_line(aes(linetype = series), alpha = 0.7) +
    scale_linetype_manual(values = c("solid", rep("dashed", length(forecasts_list)))) +
    labs(
      title = "Forecast Comparison: All Models",
      x = "Time Index",
      y = "ED Arrivals",
      color = "Series",
      linetype = "Series"
    ) +
    theme_publication
  
  # Zoomed view
  plot_data_zoom <- plot_data_long %>% filter(time_index %in% zoom_range)
  
  p2 <- ggplot(plot_data_zoom, aes(x = time_index, y = value, color = series)) +
    geom_line(aes(linetype = series), size = 0.8, alpha = 0.8) +
    geom_point(size = 1.5, alpha = 0.6) +
    scale_linetype_manual(values = c("solid", rep("dashed", length(forecasts_list)))) +
    labs(
      title = paste("Forecast Comparison: First", length(zoom_range), "Hours"),
      x = "Time Index",
      y = "ED Arrivals",
      color = "Series",
      linetype = "Series"
    ) +
    theme_publication
  
  return(list(full = p1, zoom = p2))
}

# =============================================================================
# 2. Model Accuracy Comparison
# =============================================================================

plot_accuracy_comparison <- function(accuracy_df) {
  # Bar plot for different metrics
  accuracy_long <- accuracy_df %>%
    select(Model, MAE, RMSE, MAPE) %>%
    pivot_longer(cols = c(MAE, RMSE, MAPE), names_to = "Metric", values_to = "Value")
  
  p1 <- ggplot(accuracy_long, aes(x = Model, y = Value, fill = Model)) +
    geom_col() +
    facet_wrap(~Metric, scales = "free_y") +
    labs(
      title = "Forecast Accuracy Metrics by Model",
      x = "Model",
      y = "Value"
    ) +
    theme_publication +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Ranking visualization
  p2 <- accuracy_df %>%
    select(Model, Rank_MAE, Rank_RMSE, Rank_MAPE) %>%
    pivot_longer(cols = starts_with("Rank"), names_to = "Metric", values_to = "Rank") %>%
    mutate(Metric = gsub("Rank_", "", Metric)) %>%
    ggplot(aes(x = Metric, y = Rank, group = Model, color = Model)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    scale_y_reverse() +
    labs(
      title = "Model Rankings Across Metrics",
      x = "Metric",
      y = "Rank (1 = Best)"
    ) +
    theme_publication
  
  return(list(accuracy = p1, ranking = p2))
}

# =============================================================================
# 3. Staffing Decision Performance
# =============================================================================

plot_staffing_performance <- function(eval_details, decision_name = "Decision Rule") {
  # Create time series plot
  plot_data <- eval_details %>%
    mutate(time_index = row_number()) %>%
    select(time_index, actual_required, staffing_decision, overstaffing, understaffing)
  
  # Staffing levels over time
  p1 <- ggplot(plot_data, aes(x = time_index)) +
    geom_line(aes(y = actual_required, color = "Actual Required"), size = 0.8) +
    geom_line(aes(y = staffing_decision, color = "Staffing Decision"), size = 0.8, linetype = "dashed") +
    labs(
      title = paste("Staffing Levels:", decision_name),
      x = "Time Index",
      y = "Number of Staff",
      color = "Series"
    ) +
    theme_publication
  
  # Staffing errors
  error_data <- plot_data %>%
    select(time_index, overstaffing, understaffing) %>%
    pivot_longer(cols = c(overstaffing, understaffing), 
                 names_to = "Error_Type", values_to = "Staff_Count")
  
  p2 <- ggplot(error_data, aes(x = time_index, y = Staff_Count, fill = Error_Type)) +
    geom_area(alpha = 0.6) +
    scale_fill_manual(values = c("overstaffing" = "blue", "understaffing" = "red")) +
    labs(
      title = "Staffing Errors Over Time",
      x = "Time Index",
      y = "Staff Count",
      fill = "Error Type"
    ) +
    theme_publication
  
  # Cost breakdown
  cost_data <- eval_details %>%
    summarise(
      Overstaffing = sum(cost_over),
      Understaffing = sum(cost_under),
      Base_Staffing = sum(staffing_cost)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Cost_Type", values_to = "Cost")
  
  p3 <- ggplot(cost_data, aes(x = "", y = Cost, fill = Cost_Type)) +
    geom_col() +
    coord_polar(theta = "y") +
    labs(
      title = "Total Cost Breakdown",
      fill = "Cost Type"
    ) +
    theme_publication +
    theme(axis.title = element_blank(), axis.text = element_blank())
  
  return(list(staffing_levels = p1, errors = p2, costs = p3))
}

# =============================================================================
# 4. Decision Rule Comparison
# =============================================================================

plot_decision_rule_comparison <- function(comparison_df) {
  # Service level comparison
  p1 <- ggplot(comparison_df, aes(x = reorder(Rule, Service_Level), y = Service_Level, fill = Rule)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Service Level by Decision Rule",
      x = "Decision Rule",
      y = "Service Level (%)"
    ) +
    theme_publication +
    theme(legend.position = "none")
  
  # Total cost comparison
  p2 <- ggplot(comparison_df, aes(x = reorder(Rule, -Total_Cost), y = Total_Cost, fill = Rule)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Total Cost by Decision Rule",
      x = "Decision Rule",
      y = "Total Cost ($)"
    ) +
    theme_publication +
    theme(legend.position = "none")
  
  # Scatter plot: service level vs cost
  p3 <- ggplot(comparison_df, aes(x = Service_Level, y = Total_Cost, label = Rule)) +
    geom_point(size = 4, alpha = 0.7, color = "steelblue") +
    geom_text(hjust = -0.1, vjust = 0, size = 3) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Service Level vs Total Cost Trade-off",
      x = "Service Level (%)",
      y = "Total Cost ($)"
    ) +
    theme_publication
  
  return(list(service_level = p1, total_cost = p2, tradeoff = p3))
}

# =============================================================================
# 5. Residual Diagnostics
# =============================================================================

plot_residual_diagnostics <- function(residuals, model_name = "Model") {
  # Remove NA values
  residuals <- residuals[!is.na(residuals)]
  
  # Time series plot
  p1 <- ggplot(data.frame(index = seq_along(residuals), residual = residuals),
               aes(x = index, y = residual)) +
    geom_line(color = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Residuals Over Time:", model_name),
      x = "Time Index",
      y = "Residual"
    ) +
    theme_publication
  
  # Histogram
  p2 <- ggplot(data.frame(residual = residuals), aes(x = residual)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Residual Distribution",
      x = "Residual",
      y = "Frequency"
    ) +
    theme_publication
  
  # Q-Q plot
  p3 <- ggplot(data.frame(residual = residuals), aes(sample = residual)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_publication
  
  # ACF plot
  acf_data <- acf(residuals, plot = FALSE)
  acf_df <- data.frame(
    lag = acf_data$lag[-1],
    acf = acf_data$acf[-1]
  )
  
  ci <- qnorm(0.975) / sqrt(length(residuals))
  
  p4 <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = lag, yend = 0)) +
    labs(
      title = "Autocorrelation Function",
      x = "Lag",
      y = "ACF"
    ) +
    theme_publication
  
  return(list(time_series = p1, histogram = p2, qq_plot = p3, acf = p4))
}

# =============================================================================
# 6. Feature Importance (for ML models)
# =============================================================================

plot_feature_importance <- function(importance_df, top_n = 15, model_name = "Model") {
  # Select top N features
  top_features <- importance_df %>%
    arrange(desc(Importance)) %>%
    head(top_n)
  
  p <- ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Feature Importance:", model_name),
      x = "Feature",
      y = "Importance"
    ) +
    theme_publication
  
  return(p)
}

# =============================================================================
# 7. Main Execution
# =============================================================================

# Note: Uncomment and run when you have all results
# 
# # Load data
# test_data <- readRDS("data/processed/train_test_split.rds")$test
# 
# # Load forecasts
# forecasts_list <- list(
#   ARIMA = as.numeric(readRDS("models/arima_model.rds")$forecasts$mean),
#   ETS = as.numeric(readRDS("models/ets_model.rds")$forecasts$mean),
#   RF = readRDS("models/rf_model.rds")$test_predictions,
#   XGBoost = readRDS("models/xgboost_model.rds")$test_predictions
# )
# 
# # Create forecast comparison plots
# cat("Creating forecast comparison plots...\n")
# fc_plots <- plot_forecast_comparison(test_data, forecasts_list)
# ggsave("figures/10_forecast_comparison_full.png", fc_plots$full, width = 12, height = 6)
# ggsave("figures/11_forecast_comparison_zoom.png", fc_plots$zoom, width = 12, height = 6)
# 
# # Load and plot accuracy comparison
# accuracy_df <- read_csv("results/forecast_accuracy_comparison.csv")
# acc_plots <- plot_accuracy_comparison(accuracy_df)
# ggsave("figures/12_accuracy_comparison.png", acc_plots$accuracy, width = 12, height = 8)
# ggsave("figures/13_model_rankings.png", acc_plots$ranking, width = 10, height = 6)
# 
# # Load and plot decision rule comparison
# decision_comparison <- read_csv("results/staffing_decision_comparison.csv")
# dec_plots <- plot_decision_rule_comparison(decision_comparison)
# ggsave("figures/14_service_level_comparison.png", dec_plots$service_level, width = 10, height = 6)
# ggsave("figures/15_cost_comparison.png", dec_plots$total_cost, width = 10, height = 6)
# ggsave("figures/16_service_cost_tradeoff.png", dec_plots$tradeoff, width = 10, height = 6)
# 
# cat("\n=============================================================================\n")
# cat("Visualization Complete!\n")
# cat("All figures saved to figures/\n")
# cat("=============================================================================\n")

cat("\n=============================================================================\n")
cat("Visualization Script Ready\n")
cat("Please run previous scripts to generate data and results\n")
cat("=============================================================================\n")
