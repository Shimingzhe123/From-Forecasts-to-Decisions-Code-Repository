# =============================================================================
# 05_performance_evaluation.R
# From Forecasts to Decisions: ED Staffing Analysis
# 
# This script evaluates forecast accuracy and operational performance
# =============================================================================

# Load required libraries
library(tidyverse)
library(knitr)
library(kableExtra)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# Create output directories
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

# =============================================================================
# 1. Forecast Accuracy Metrics
# =============================================================================

calculate_accuracy_metrics <- function(actual, forecast, model_name = "Model") {
  # Remove NA values
  valid_idx <- !is.na(actual) & !is.na(forecast)
  actual <- actual[valid_idx]
  forecast <- forecast[valid_idx]
  
  # Error
  error <- actual - forecast
  
  # Mean Error (ME) - measures bias
  me <- mean(error)
  
  # Mean Absolute Error (MAE)
  mae <- mean(abs(error))
  
  # Root Mean Square Error (RMSE)
  rmse <- sqrt(mean(error^2))
  
  # Mean Absolute Percentage Error (MAPE)
  mape <- mean(abs(error / actual)) * 100
  
  # Symmetric MAPE
  smape <- mean(2 * abs(error) / (abs(actual) + abs(forecast))) * 100
  
  # Mean Absolute Scaled Error (MASE)
  # Using naive forecast as benchmark
  naive_error <- diff(actual)
  mase <- mae / mean(abs(naive_error))
  
  # R-squared
  ss_res <- sum(error^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # Adjusted R-squared (assuming p=1 predictor)
  n <- length(actual)
  p <- 1
  adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)
  
  return(data.frame(
    Model = model_name,
    ME = me,
    MAE = mae,
    RMSE = rmse,
    MAPE = mape,
    SMAPE = smape,
    MASE = mase,
    R_squared = r_squared,
    Adj_R_squared = adj_r_squared,
    n_obs = n
  ))
}

# =============================================================================
# 2. Operational Performance Metrics
# =============================================================================

calculate_operational_metrics <- function(staffing_eval_details) {
  # Service level: % of time adequately staffed
  service_level <- mean(staffing_eval_details$understaffing == 0) * 100
  
  # Average staffing levels
  avg_staff_scheduled <- mean(staffing_eval_details$staffing_decision)
  avg_staff_required <- mean(staffing_eval_details$actual_required)
  
  # Staffing accuracy
  avg_overstaffing <- mean(staffing_eval_details$overstaffing)
  avg_understaffing <- mean(staffing_eval_details$understaffing)
  avg_absolute_error <- mean(abs(staffing_eval_details$staffing_decision - 
                                  staffing_eval_details$actual_required))
  
  # Cost metrics
  total_cost <- sum(staffing_eval_details$total_cost)
  avg_cost_per_hour <- mean(staffing_eval_details$total_cost)
  total_overstaffing_cost <- sum(staffing_eval_details$cost_over)
  total_understaffing_cost <- sum(staffing_eval_details$cost_under)
  total_staffing_cost <- sum(staffing_eval_details$staffing_cost)
  
  # Cost breakdown percentages
  pct_overstaffing_cost <- (total_overstaffing_cost / total_cost) * 100
  pct_understaffing_cost <- (total_understaffing_cost / total_cost) * 100
  pct_staffing_cost <- (total_staffing_cost / total_cost) * 100
  
  return(data.frame(
    Service_Level_Pct = service_level,
    Avg_Staff_Scheduled = avg_staff_scheduled,
    Avg_Staff_Required = avg_staff_required,
    Avg_Overstaffing = avg_overstaffing,
    Avg_Understaffing = avg_understaffing,
    Avg_Absolute_Error = avg_absolute_error,
    Total_Cost = total_cost,
    Avg_Cost_Per_Hour = avg_cost_per_hour,
    Total_Overstaffing_Cost = total_overstaffing_cost,
    Total_Understaffing_Cost = total_understaffing_cost,
    Total_Staffing_Cost = total_staffing_cost,
    Pct_Overstaffing_Cost = pct_overstaffing_cost,
    Pct_Understaffing_Cost = pct_understaffing_cost,
    Pct_Staffing_Cost = pct_staffing_cost
  ))
}

# =============================================================================
# 3. Model Comparison
# =============================================================================

compare_models <- function(models_list, actual, test_data) {
  comparison <- list()
  
  for (model_name in names(models_list)) {
    model <- models_list[[model_name]]
    
    # Get forecast depending on model type
    if (model_name %in% c("ARIMA", "ETS", "Seasonal_Naive")) {
      forecast <- as.numeric(model$forecasts$mean)
    } else {
      # For ML models (RF, XGBoost)
      forecast <- model$test_predictions
    }
    
    # Calculate accuracy metrics
    accuracy <- calculate_accuracy_metrics(actual, forecast, model_name)
    comparison[[model_name]] <- accuracy
  }
  
  # Combine all results
  comparison_df <- do.call(rbind, comparison)
  
  # Rank models
  comparison_df <- comparison_df %>%
    arrange(RMSE) %>%
    mutate(Rank_RMSE = row_number()) %>%
    arrange(MAE) %>%
    mutate(Rank_MAE = row_number()) %>%
    arrange(MAPE) %>%
    mutate(Rank_MAPE = row_number()) %>%
    mutate(Avg_Rank = (Rank_RMSE + Rank_MAE + Rank_MAPE) / 3) %>%
    arrange(Avg_Rank)
  
  return(comparison_df)
}

# =============================================================================
# 4. Forecast vs Decision Performance
# =============================================================================

analyze_forecast_decision_relationship <- function(accuracy_metrics, operational_metrics) {
  # Combine accuracy and operational metrics
  combined <- cbind(
    accuracy_metrics %>% select(Model, MAE, RMSE, MAPE),
    operational_metrics %>% select(Service_Level_Pct, Total_Cost, Avg_Absolute_Error)
  )
  
  # Calculate correlations
  correlations <- data.frame(
    Metric_Pair = c(
      "MAE vs Total_Cost",
      "RMSE vs Total_Cost", 
      "MAPE vs Total_Cost",
      "MAE vs Service_Level",
      "RMSE vs Service_Level",
      "MAPE vs Service_Level"
    ),
    Correlation = c(
      cor(combined$MAE, combined$Total_Cost),
      cor(combined$RMSE, combined$Total_Cost),
      cor(combined$MAPE, combined$Total_Cost),
      cor(combined$MAE, combined$Service_Level_Pct),
      cor(combined$RMSE, combined$Service_Level_Pct),
      cor(combined$MAPE, combined$Service_Level_Pct)
    )
  )
  
  return(list(
    combined_metrics = combined,
    correlations = correlations
  ))
}

# =============================================================================
# 5. Residual Analysis
# =============================================================================

analyze_residuals <- function(residuals, model_name = "Model") {
  # Remove NA values
  residuals <- residuals[!is.na(residuals)]
  
  # Basic statistics
  mean_residual <- mean(residuals)
  sd_residual <- sd(residuals)
  
  # Test for autocorrelation
  acf_values <- acf(residuals, plot = FALSE)$acf[2:11]  # First 10 lags
  
  # Test for normality (Shapiro-Wilk test)
  if (length(residuals) <= 5000) {
    normality_test <- shapiro.test(residuals)
    p_value_normality <- normality_test$p.value
  } else {
    p_value_normality <- NA  # Test not applicable for large samples
  }
  
  # Ljung-Box test for autocorrelation
  lb_test <- Box.test(residuals, lag = 10, type = "Ljung-Box")
  p_value_lb <- lb_test$p.value
  
  return(data.frame(
    Model = model_name,
    Mean_Residual = mean_residual,
    SD_Residual = sd_residual,
    P_Value_Normality = p_value_normality,
    P_Value_LjungBox = p_value_lb
  ))
}

# =============================================================================
# 6. Time-Based Performance Analysis
# =============================================================================

analyze_performance_by_time <- function(eval_details, datetime_col) {
  # Add time features
  eval_details <- eval_details %>%
    mutate(
      datetime = datetime_col,
      hour = hour(datetime),
      weekday = wday(datetime, label = TRUE),
      is_weekend = weekday %in% c("Sat", "Sun")
    )
  
  # Performance by hour
  hourly_perf <- eval_details %>%
    group_by(hour) %>%
    summarise(
      avg_understaffing = mean(understaffing),
      avg_overstaffing = mean(overstaffing),
      service_level = mean(understaffing == 0) * 100,
      avg_cost = mean(total_cost),
      .groups = "drop"
    )
  
  # Performance by weekday
  weekday_perf <- eval_details %>%
    group_by(weekday) %>%
    summarise(
      avg_understaffing = mean(understaffing),
      avg_overstaffing = mean(overstaffing),
      service_level = mean(understaffing == 0) * 100,
      avg_cost = mean(total_cost),
      .groups = "drop"
    )
  
  # Weekend vs weekday
  weekend_comparison <- eval_details %>%
    group_by(is_weekend) %>%
    summarise(
      avg_understaffing = mean(understaffing),
      avg_overstaffing = mean(overstaffing),
      service_level = mean(understaffing == 0) * 100,
      avg_cost = mean(total_cost),
      .groups = "drop"
    ) %>%
    mutate(period = ifelse(is_weekend, "Weekend", "Weekday"))
  
  return(list(
    hourly = hourly_perf,
    weekday = weekday_perf,
    weekend_comparison = weekend_comparison
  ))
}

# =============================================================================
# 7. Generate Summary Report
# =============================================================================

generate_summary_report <- function(results_list, output_file = "results/summary_report.txt") {
  sink(output_file)
  
  cat("=============================================================================\n")
  cat("FORECAST AND STAFFING DECISION PERFORMANCE SUMMARY\n")
  cat("=============================================================================\n\n")
  
  cat("1. FORECAST ACCURACY COMPARISON\n")
  cat("-------------------------------------------\n")
  print(kable(results_list$accuracy_comparison, digits = 3))
  cat("\n\n")
  
  cat("2. OPERATIONAL PERFORMANCE\n")
  cat("-------------------------------------------\n")
  print(kable(results_list$operational_performance, digits = 3))
  cat("\n\n")
  
  cat("3. FORECAST-DECISION RELATIONSHIP\n")
  cat("-------------------------------------------\n")
  print(kable(results_list$forecast_decision_analysis$correlations, digits = 3))
  cat("\n\n")
  
  sink()
  
  cat("Summary report saved to:", output_file, "\n")
}

# =============================================================================
# 8. Main Execution
# =============================================================================

# Note: Uncomment and run when you have all results
# 
# # Load data and models
# test_data <- readRDS("data/processed/train_test_split.rds")$test
# actual_arrivals <- test_data$arrivals
# 
# models_list <- list(
#   ARIMA = readRDS("models/arima_model.rds"),
#   ETS = readRDS("models/ets_model.rds"),
#   Seasonal_Naive = readRDS("models/snaive_model.rds"),
#   Random_Forest = readRDS("models/rf_model.rds"),
#   XGBoost = readRDS("models/xgboost_model.rds")
# )
# 
# # Compare forecast accuracy
# cat("Comparing forecast accuracy...\n")
# accuracy_comparison <- compare_models(models_list, actual_arrivals, test_data)
# write_csv(accuracy_comparison, "results/forecast_accuracy_comparison.csv")
# 
# # Analyze operational performance
# # (requires staffing evaluation results from script 04)
# 
# cat("\n=============================================================================\n")
# cat("Performance Evaluation Complete!\n")
# cat("Results saved to results/\n")
# cat("=============================================================================\n")

cat("\n=============================================================================\n")
cat("Performance Evaluation Script Ready\n")
cat("Please run previous scripts to generate models and forecasts\n")
cat("=============================================================================\n")
