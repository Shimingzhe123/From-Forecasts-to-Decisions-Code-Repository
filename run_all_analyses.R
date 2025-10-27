# =============================================================================
# run_all_analyses.R
# From Forecasts to Decisions: ED Staffing Analysis
# 
# Master script to run all analyses in sequence
# =============================================================================

# Set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("=============================================================================\n")
cat("FROM FORECASTS TO DECISIONS: ED STAFFING ANALYSIS\n")
cat("Master Script - Running All Analyses\n")
cat("=============================================================================\n\n")

# Record start time
start_time <- Sys.time()

# =============================================================================
# Step 0: Install Required Packages
# =============================================================================

cat("\n--- Step 0: Checking Required Packages ---\n")
source("install_packages.R")

# =============================================================================
# Step 1: Data Preparation
# =============================================================================

cat("\n=============================================================================\n")
cat("--- Step 1: Data Preparation ---\n")
cat("=============================================================================\n")

tryCatch({
  source("scripts/01_data_preparation.R")
  cat("✓ Data preparation completed successfully\n")
}, error = function(e) {
  cat("✗ Error in data preparation:\n")
  cat(paste("  ", e$message, "\n"))
  cat("Please ensure your data is in the correct format and location.\n")
  cat("See data/DATA_FORMAT.md for details.\n")
  stop("Analysis stopped due to data preparation error.")
})

# =============================================================================
# Step 2: Exploratory Analysis
# =============================================================================

cat("\n=============================================================================\n")
cat("--- Step 2: Exploratory Analysis ---\n")
cat("=============================================================================\n")

tryCatch({
  source("scripts/02_exploratory_analysis.R")
  cat("✓ Exploratory analysis completed successfully\n")
}, error = function(e) {
  cat("✗ Error in exploratory analysis:\n")
  cat(paste("  ", e$message, "\n"))
  stop("Analysis stopped due to exploratory analysis error.")
})

# =============================================================================
# Step 3: Forecasting Models
# =============================================================================

cat("\n=============================================================================\n")
cat("--- Step 3: Training Forecasting Models ---\n")
cat("=============================================================================\n")

tryCatch({
  source("scripts/03_forecasting_models.R")
  cat("✓ Forecasting models completed successfully\n")
}, error = function(e) {
  cat("✗ Error in forecasting models:\n")
  cat(paste("  ", e$message, "\n"))
  stop("Analysis stopped due to forecasting models error.")
})

# =============================================================================
# Step 4: Staffing Decisions
# =============================================================================

cat("\n=============================================================================\n")
cat("--- Step 4: Staffing Decision Analysis ---\n")
cat("=============================================================================\n")

tryCatch({
  source("scripts/04_staffing_decisions.R")
  cat("✓ Staffing decisions completed successfully\n")
}, error = function(e) {
  cat("✗ Error in staffing decisions:\n")
  cat(paste("  ", e$message, "\n"))
  stop("Analysis stopped due to staffing decisions error.")
})

# =============================================================================
# Step 5: Performance Evaluation
# =============================================================================

cat("\n=============================================================================\n")
cat("--- Step 5: Performance Evaluation ---\n")
cat("=============================================================================\n")

tryCatch({
  source("scripts/05_performance_evaluation.R")
  cat("✓ Performance evaluation completed successfully\n")
}, error = function(e) {
  cat("✗ Error in performance evaluation:\n")
  cat(paste("  ", e$message, "\n"))
  stop("Analysis stopped due to performance evaluation error.")
})

# =============================================================================
# Step 6: Visualization
# =============================================================================

cat("\n=============================================================================\n")
cat("--- Step 6: Creating Visualizations ---\n")
cat("=============================================================================\n")

tryCatch({
  source("scripts/06_visualization.R")
  cat("✓ Visualization completed successfully\n")
}, error = function(e) {
  cat("✗ Error in visualization:\n")
  cat(paste("  ", e$message, "\n"))
  stop("Analysis stopped due to visualization error.")
})

# =============================================================================
# Summary
# =============================================================================

end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "mins")

cat("\n=============================================================================\n")
cat("ALL ANALYSES COMPLETED SUCCESSFULLY!\n")
cat("=============================================================================\n\n")

cat("Summary:\n")
cat(sprintf("  Total execution time: %.2f minutes\n", elapsed_time))
cat("\nGenerated outputs:\n")
cat("  - Processed data: data/processed/\n")
cat("  - Trained models: models/\n")
cat("  - Results and metrics: results/\n")
cat("  - Figures and plots: figures/\n")

cat("\nNext steps:\n")
cat("  1. Review descriptive statistics in results/\n")
cat("  2. Examine figures in figures/\n")
cat("  3. Compare model performance in results/model_comparison.csv\n")
cat("  4. Analyze staffing decisions in results/staffing_decision_comparison.csv\n")
cat("  5. Use findings in your research paper or presentation\n")

cat("\n=============================================================================\n")
cat("Thank you for using the From Forecasts to Decisions Code Repository!\n")
cat("=============================================================================\n")
