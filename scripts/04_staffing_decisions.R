# =============================================================================
# 04_staffing_decisions.R
# From Forecasts to Decisions: ED Staffing Analysis
# 
# This script implements staffing decision rules based on forecasts
# =============================================================================

# Load required libraries
library(tidyverse)
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
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

# =============================================================================
# 1. Staffing Parameters
# =============================================================================

# Define staffing parameters (adjust based on your ED context)
STAFFING_PARAMS <- list(
  patients_per_nurse = 4,      # Average patients per nurse
  patients_per_doctor = 8,     # Average patients per doctor
  min_staff_nurses = 2,        # Minimum nurses per shift
  min_staff_doctors = 1,       # Minimum doctors per shift
  shift_length_hours = 8,      # Shift length
  handoff_time_hours = 1,      # Overlap for handoff
  cost_understaffing = 100,    # Cost per understaffed hour
  cost_overstaffing = 50,      # Cost per overstaffed hour
  cost_regular_hour = 40,      # Regular wage per hour
  cost_overtime_hour = 60      # Overtime wage per hour
)

# =============================================================================
# 2. Calculate Staffing Requirements
# =============================================================================

calculate_staffing_requirements <- function(forecasted_arrivals, params = STAFFING_PARAMS) {
  # Calculate required staff based on forecasted arrivals
  nurses_required <- pmax(
    ceiling(forecasted_arrivals / params$patients_per_nurse),
    params$min_staff_nurses
  )
  
  doctors_required <- pmax(
    ceiling(forecasted_arrivals / params$patients_per_doctor),
    params$min_staff_doctors
  )
  
  total_staff_required <- nurses_required + doctors_required
  
  return(data.frame(
    forecasted_arrivals = forecasted_arrivals,
    nurses_required = nurses_required,
    doctors_required = doctors_required,
    total_staff_required = total_staff_required
  ))
}

# =============================================================================
# 3. Staffing Decision Rules
# =============================================================================

# Rule 1: Direct Forecast (DF)
# Staff exactly according to the forecast
apply_direct_forecast_rule <- function(forecast, params = STAFFING_PARAMS) {
  staffing_req <- calculate_staffing_requirements(forecast, params)
  return(staffing_req)
}

# Rule 2: Safety Stock (SS)
# Add buffer to forecast to account for uncertainty
apply_safety_stock_rule <- function(forecast, buffer_percent = 0.1, params = STAFFING_PARAMS) {
  adjusted_forecast <- forecast * (1 + buffer_percent)
  staffing_req <- calculate_staffing_requirements(adjusted_forecast, params)
  return(staffing_req)
}

# Rule 3: Quantile-Based (QB)
# Staff for a specific quantile of the forecast distribution
apply_quantile_rule <- function(forecast, forecast_sd, quantile = 0.8, params = STAFFING_PARAMS) {
  # Assuming normal distribution
  z_score <- qnorm(quantile)
  adjusted_forecast <- forecast + z_score * forecast_sd
  staffing_req <- calculate_staffing_requirements(adjusted_forecast, params)
  return(staffing_req)
}

# Rule 4: Cost-Sensitive (CS)
# Optimize staffing based on cost trade-offs
apply_cost_sensitive_rule <- function(forecast, forecast_sd, params = STAFFING_PARAMS) {
  # Calculate optimal service level based on cost ratio
  cost_ratio <- params$cost_understaffing / (params$cost_understaffing + params$cost_overstaffing)
  
  z_score <- qnorm(cost_ratio)
  adjusted_forecast <- forecast + z_score * forecast_sd
  staffing_req <- calculate_staffing_requirements(adjusted_forecast, params)
  
  return(staffing_req)
}

# Rule 5: Rolling Average (RA)
# Use historical average instead of forecast
apply_rolling_average_rule <- function(historical_arrivals, window = 168, params = STAFFING_PARAMS) {
  # 168 hours = 1 week
  avg_arrivals <- zoo::rollmean(historical_arrivals, k = window, fill = NA, align = "right")
  staffing_req <- calculate_staffing_requirements(avg_arrivals, params)
  return(staffing_req)
}

# =============================================================================
# 4. Evaluate Staffing Decisions
# =============================================================================

evaluate_staffing_decision <- function(actual_arrivals, staffing_decision, params = STAFFING_PARAMS) {
  # Calculate actual required staff
  actual_required <- calculate_staffing_requirements(actual_arrivals, params)
  
  # Calculate staffing errors
  staff_difference <- staffing_decision$total_staff_required - actual_required$total_staff_required
  
  # Overstaffing and understaffing
  overstaffing <- pmax(staff_difference, 0)
  understaffing <- pmax(-staff_difference, 0)
  
  # Calculate costs
  cost_over <- overstaffing * params$cost_overstaffing
  cost_under <- understaffing * params$cost_understaffing
  staffing_cost <- staffing_decision$total_staff_required * params$cost_regular_hour
  
  total_cost <- cost_over + cost_under + staffing_cost
  
  # Performance metrics
  service_level <- mean(staff_difference >= 0)  # Percentage of time adequately staffed
  avg_overstaffing <- mean(overstaffing)
  avg_understaffing <- mean(understaffing)
  avg_total_cost <- mean(total_cost)
  
  return(list(
    details = data.frame(
      actual_arrivals = actual_arrivals,
      actual_required = actual_required$total_staff_required,
      staffing_decision = staffing_decision$total_staff_required,
      overstaffing = overstaffing,
      understaffing = understaffing,
      cost_over = cost_over,
      cost_under = cost_under,
      staffing_cost = staffing_cost,
      total_cost = total_cost
    ),
    summary = data.frame(
      service_level = service_level,
      avg_overstaffing = avg_overstaffing,
      avg_understaffing = avg_understaffing,
      avg_total_cost = avg_total_cost,
      total_cost = sum(total_cost)
    )
  ))
}

# =============================================================================
# 5. Compare Decision Rules
# =============================================================================

compare_decision_rules <- function(actual, forecast, forecast_sd = NULL, params = STAFFING_PARAMS) {
  # Apply different decision rules
  df_staffing <- apply_direct_forecast_rule(forecast, params)
  ss_staffing <- apply_safety_stock_rule(forecast, buffer_percent = 0.1, params)
  
  # For rules requiring forecast SD
  if (!is.null(forecast_sd)) {
    qb_staffing <- apply_quantile_rule(forecast, forecast_sd, quantile = 0.8, params)
    cs_staffing <- apply_cost_sensitive_rule(forecast, forecast_sd, params)
  }
  
  # Evaluate each rule
  df_eval <- evaluate_staffing_decision(actual, df_staffing, params)
  ss_eval <- evaluate_staffing_decision(actual, ss_staffing, params)
  
  results <- data.frame(
    Rule = c("Direct_Forecast", "Safety_Stock"),
    Service_Level = c(df_eval$summary$service_level, ss_eval$summary$service_level),
    Avg_Overstaffing = c(df_eval$summary$avg_overstaffing, ss_eval$summary$avg_overstaffing),
    Avg_Understaffing = c(df_eval$summary$avg_understaffing, ss_eval$summary$avg_understaffing),
    Avg_Total_Cost = c(df_eval$summary$avg_total_cost, ss_eval$summary$avg_total_cost),
    Total_Cost = c(df_eval$summary$total_cost, ss_eval$summary$total_cost)
  )
  
  if (!is.null(forecast_sd)) {
    qb_eval <- evaluate_staffing_decision(actual, qb_staffing, params)
    cs_eval <- evaluate_staffing_decision(actual, cs_staffing, params)
    
    results <- rbind(
      results,
      data.frame(
        Rule = c("Quantile_Based", "Cost_Sensitive"),
        Service_Level = c(qb_eval$summary$service_level, cs_eval$summary$service_level),
        Avg_Overstaffing = c(qb_eval$summary$avg_overstaffing, cs_eval$summary$avg_overstaffing),
        Avg_Understaffing = c(qb_eval$summary$avg_understaffing, cs_eval$summary$avg_understaffing),
        Avg_Total_Cost = c(qb_eval$summary$avg_total_cost, cs_eval$summary$avg_total_cost),
        Total_Cost = c(qb_eval$summary$total_cost, cs_eval$summary$total_cost)
      )
    )
  }
  
  return(results)
}

# =============================================================================
# 6. Sensitivity Analysis
# =============================================================================

sensitivity_analysis_costs <- function(actual, forecast, cost_ratios = seq(0.5, 2, 0.1)) {
  results <- list()
  
  base_params <- STAFFING_PARAMS
  
  for (i in seq_along(cost_ratios)) {
    params <- base_params
    params$cost_understaffing <- base_params$cost_understaffing * cost_ratios[i]
    
    staffing <- apply_direct_forecast_rule(forecast, params)
    eval_result <- evaluate_staffing_decision(actual, staffing, params)
    
    results[[i]] <- data.frame(
      cost_ratio = cost_ratios[i],
      service_level = eval_result$summary$service_level,
      total_cost = eval_result$summary$total_cost
    )
  }
  
  return(do.call(rbind, results))
}

# =============================================================================
# 7. Main Execution
# =============================================================================

# Note: Uncomment and run when you have forecasts
# 
# # Load test data and forecasts
# test_data <- readRDS("data/processed/train_test_split.rds")$test
# arima_results <- readRDS("models/arima_model.rds")
# 
# # Extract actual values and forecasts
# actual_arrivals <- test_data$arrivals
# forecasted_arrivals <- as.numeric(arima_results$forecasts$mean)
# forecast_sd <- as.numeric(arima_results$forecasts$upper[,1] - arima_results$forecasts$mean) / 1.96
# 
# # Compare decision rules
# cat("Comparing staffing decision rules...\n")
# comparison_results <- compare_decision_rules(actual_arrivals, forecasted_arrivals, forecast_sd)
# write_csv(comparison_results, "results/staffing_decision_comparison.csv")
# 
# # Run sensitivity analysis
# cat("Running sensitivity analysis...\n")
# sensitivity_results <- sensitivity_analysis_costs(actual_arrivals, forecasted_arrivals)
# write_csv(sensitivity_results, "results/sensitivity_analysis_costs.csv")
# 
# cat("\n=============================================================================\n")
# cat("Staffing Decision Analysis Complete!\n")
# cat("Results saved to results/\n")
# cat("=============================================================================\n")

cat("\n=============================================================================\n")
cat("Staffing Decisions Script Ready\n")
cat("Please run previous scripts first to generate forecasts\n")
cat("=============================================================================\n")
