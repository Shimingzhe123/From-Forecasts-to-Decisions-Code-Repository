# =============================================================================
# 03_forecasting_models.R
# From Forecasts to Decisions: ED Staffing Analysis
# 
# This script implements various forecasting models for ED arrivals
# =============================================================================

# Load required libraries
library(tidyverse)
library(forecast)
library(tseries)
library(caret)
library(randomForest)
library(xgboost)

# Set working directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd("..")
} else {
  # If not in RStudio, assume we're already in project root
  cat("Note: Not running in RStudio. Ensure working directory is set to project root.\n")
}

# Create output directories
if (!dir.exists("models")) dir.create("models", recursive = TRUE)
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

# =============================================================================
# 1. Load Data
# =============================================================================

# split_data <- readRDS("data/processed/train_test_split.rds")
# train_data <- split_data$train
# test_data <- split_data$test

# =============================================================================
# 2. ARIMA Models
# =============================================================================

fit_arima_model <- function(train_ts, forecast_horizon = 24) {
  # Automatic ARIMA selection
  arima_model <- auto.arima(train_ts,
                           seasonal = TRUE,
                           stepwise = FALSE,
                           approximation = FALSE,
                           trace = TRUE)
  
  # Generate forecasts
  forecasts <- forecast(arima_model, h = forecast_horizon)
  
  return(list(
    model = arima_model,
    forecasts = forecasts,
    fitted = fitted(arima_model),
    residuals = residuals(arima_model)
  ))
}

# =============================================================================
# 3. Exponential Smoothing Models
# =============================================================================

fit_ets_model <- function(train_ts, forecast_horizon = 24) {
  # Automatic ETS selection
  ets_model <- ets(train_ts)
  
  # Generate forecasts
  forecasts <- forecast(ets_model, h = forecast_horizon)
  
  return(list(
    model = ets_model,
    forecasts = forecasts,
    fitted = fitted(ets_model),
    residuals = residuals(ets_model)
  ))
}

# =============================================================================
# 4. Seasonal Naive Baseline
# =============================================================================

fit_snaive_model <- function(train_ts, forecast_horizon = 24) {
  # Seasonal naive forecast
  forecasts <- snaive(train_ts, h = forecast_horizon)
  
  return(list(
    forecasts = forecasts,
    fitted = fitted(forecasts),
    residuals = residuals(forecasts)
  ))
}

# =============================================================================
# 5. Random Forest Model
# =============================================================================

fit_random_forest <- function(train_data, test_data, target_col = "arrivals") {
  # Prepare features (exclude target and datetime)
  feature_cols <- setdiff(names(train_data), c("datetime", target_col))
  
  # Remove columns with NA
  complete_cols <- feature_cols[sapply(train_data[feature_cols], function(x) !any(is.na(x)))]
  
  # Train model
  rf_model <- randomForest(
    x = train_data[, complete_cols],
    y = train_data[[target_col]],
    ntree = 500,
    mtry = floor(sqrt(length(complete_cols))),
    importance = TRUE,
    na.action = na.omit
  )
  
  # Generate predictions
  train_pred <- predict(rf_model, train_data[, complete_cols])
  test_pred <- predict(rf_model, test_data[, complete_cols])
  
  return(list(
    model = rf_model,
    train_predictions = train_pred,
    test_predictions = test_pred,
    feature_importance = importance(rf_model)
  ))
}

# =============================================================================
# 6. XGBoost Model
# =============================================================================

fit_xgboost <- function(train_data, test_data, target_col = "arrivals") {
  # Prepare features
  feature_cols <- setdiff(names(train_data), c("datetime", target_col))
  complete_cols <- feature_cols[sapply(train_data[feature_cols], function(x) !any(is.na(x)))]
  
  # Prepare matrices
  train_matrix <- xgb.DMatrix(
    data = as.matrix(train_data[, complete_cols]),
    label = train_data[[target_col]]
  )
  
  test_matrix <- xgb.DMatrix(
    data = as.matrix(test_data[, complete_cols]),
    label = test_data[[target_col]]
  )
  
  # Set parameters
  params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Train model with cross-validation
  xgb_model <- xgb.train(
    params = params,
    data = train_matrix,
    nrounds = 100,
    watchlist = list(train = train_matrix, test = test_matrix),
    early_stopping_rounds = 10,
    verbose = 1
  )
  
  # Generate predictions
  train_pred <- predict(xgb_model, train_matrix)
  test_pred <- predict(xgb_model, test_matrix)
  
  return(list(
    model = xgb_model,
    train_predictions = train_pred,
    test_predictions = test_pred,
    feature_importance = xgb.importance(complete_cols, model = xgb_model)
  ))
}

# =============================================================================
# 7. Ensemble Model
# =============================================================================

create_ensemble_forecast <- function(forecasts_list, weights = NULL) {
  # If weights not provided, use equal weights
  if (is.null(weights)) {
    weights <- rep(1 / length(forecasts_list), length(forecasts_list))
  }
  
  # Combine forecasts
  ensemble <- Reduce("+", Map("*", forecasts_list, weights))
  
  return(ensemble)
}

# =============================================================================
# 8. Model Evaluation Metrics
# =============================================================================

calculate_forecast_metrics <- function(actual, predicted) {
  # Remove NA values
  valid_idx <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid_idx]
  predicted <- predicted[valid_idx]
  
  # Calculate metrics
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  smape <- mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
  
  # R-squared
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  return(data.frame(
    MAE = mae,
    RMSE = rmse,
    MAPE = mape,
    SMAPE = smape,
    R_squared = r_squared
  ))
}

# =============================================================================
# 9. Cross-Validation
# =============================================================================

time_series_cv <- function(data, model_func, k_folds = 5, horizon = 24) {
  n <- nrow(data)
  fold_size <- floor(n / k_folds)
  
  cv_results <- list()
  
  for (i in 1:(k_folds - 1)) {
    train_end <- fold_size * i
    test_end <- min(train_end + horizon, n)
    
    train_fold <- data[1:train_end, ]
    test_fold <- data[(train_end + 1):test_end, ]
    
    # Fit model and evaluate
    model <- model_func(train_fold)
    predictions <- predict(model, test_fold)
    
    metrics <- calculate_forecast_metrics(test_fold$arrivals, predictions)
    cv_results[[i]] <- metrics
  }
  
  # Average metrics across folds
  cv_summary <- do.call(rbind, cv_results) %>%
    summarise(across(everything(), mean))
  
  return(cv_summary)
}

# =============================================================================
# 10. Main Execution
# =============================================================================

# Note: Uncomment and run when you have processed data
# 
# # Load data
# split_data <- readRDS("data/processed/train_test_split.rds")
# train_data <- split_data$train
# test_data <- split_data$test
# 
# # Create time series objects for time series models
# train_ts <- ts(train_data$arrivals, frequency = 24)
# test_ts <- ts(test_data$arrivals, frequency = 24)
# 
# cat("Training forecasting models...\n\n")
# 
# # 1. ARIMA Model
# cat("1. Training ARIMA model...\n")
# arima_results <- fit_arima_model(train_ts, forecast_horizon = nrow(test_data))
# saveRDS(arima_results, "models/arima_model.rds")
# 
# # 2. ETS Model
# cat("2. Training ETS model...\n")
# ets_results <- fit_ets_model(train_ts, forecast_horizon = nrow(test_data))
# saveRDS(ets_results, "models/ets_model.rds")
# 
# # 3. Seasonal Naive
# cat("3. Training Seasonal Naive baseline...\n")
# snaive_results <- fit_snaive_model(train_ts, forecast_horizon = nrow(test_data))
# saveRDS(snaive_results, "models/snaive_model.rds")
# 
# # 4. Random Forest
# cat("4. Training Random Forest model...\n")
# rf_results <- fit_random_forest(train_data, test_data)
# saveRDS(rf_results, "models/rf_model.rds")
# 
# # 5. XGBoost
# cat("5. Training XGBoost model...\n")
# xgb_results <- fit_xgboost(train_data, test_data)
# saveRDS(xgb_results, "models/xgboost_model.rds")
# 
# # Calculate metrics for all models
# cat("\nCalculating forecast accuracy metrics...\n")
# 
# metrics_df <- data.frame(
#   Model = c("ARIMA", "ETS", "Seasonal_Naive", "Random_Forest", "XGBoost"),
#   stringsAsFactors = FALSE
# )
# 
# # Add metrics for each model
# # ... (calculate metrics for each model)
# 
# write_csv(metrics_df, "results/model_comparison.csv")
# 
# cat("\n=============================================================================\n")
# cat("Model Training Complete!\n")
# cat("Models saved to models/\n")
# cat("Results saved to results/\n")
# cat("=============================================================================\n")

cat("\n=============================================================================\n")
cat("Forecasting Models Script Ready\n")
cat("Please run previous data preparation scripts first\n")
cat("=============================================================================\n")
