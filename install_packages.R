# Package Installation Script
# From Forecasts to Decisions: ED Staffing Analysis
#
# This script installs all required R packages for the project

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(sprintf("Installing %s...\n", pkg))
      install.packages(pkg, dependencies = TRUE)
    } else {
      cat(sprintf("%s is already installed.\n", pkg))
    }
  }
}

# List of required packages
required_packages <- c(
  # Data manipulation and utilities
  "tidyverse",
  "dplyr",
  "tidyr",
  "readr",
  
  # Time series analysis
  "forecast",
  "tseries",
  "zoo",
  "lubridate",
  
  # Machine learning
  "caret",
  "randomForest",
  "xgboost",
  "glmnet",
  
  # Visualization
  "ggplot2",
  "gridExtra",
  "scales",
  
  # Reporting and tables
  "knitr",
  "kableExtra",
  
  # Additional utilities
  "rstudioapi"
)

cat("=============================================================================\n")
cat("Installing Required Packages for From Forecasts to Decisions Project\n")
cat("=============================================================================\n\n")

# Install packages
install_if_missing(required_packages)

cat("\n=============================================================================\n")
cat("Package Installation Complete!\n")
cat("=============================================================================\n")

# Display session info
cat("\nSession Information:\n")
sessionInfo()
