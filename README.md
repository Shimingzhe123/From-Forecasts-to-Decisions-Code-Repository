# From Forecasts to Decisions: Code Repository

This repository contains the R code for the research paper:

**"From Forecasts to Decisions: Assessing the Operational Value of Forecasting Accuracy in Emergency Department Staffing Decisions"**

## Overview

This project analyzes the operational value of forecasting accuracy in emergency department (ED) staffing decisions. The code implements various forecasting models and evaluates their impact on staffing decisions and operational outcomes.

## Repository Structure

```
.
├── data/              # Data files (raw and processed)
├── scripts/           # R scripts for analysis
├── results/           # Output results and tables
├── output/            # Generated output files
├── figures/           # Plots and visualizations
├── README.md          # This file
└── From-Forecasts-to-Decisions.Rproj  # RStudio project file
```

## Prerequisites

### Required R Packages

Install the required packages using:

```r
install.packages(c(
  "tidyverse",    # Data manipulation and visualization
  "forecast",     # Time series forecasting
  "tseries",      # Time series analysis
  "lubridate",    # Date and time manipulation
  "zoo",          # Time series objects
  "caret",        # Machine learning utilities
  "randomForest", # Random forest models
  "xgboost",      # Gradient boosting
  "glmnet",       # Regularized regression
  "ggplot2",      # Advanced plotting
  "gridExtra",    # Arranging plots
  "knitr",        # Report generation
  "kableExtra"    # Table formatting
))
```

## Getting Started

1. Clone this repository:
   ```bash
   git clone https://github.com/Shimingzhe123/From-Forecasts-to-Decisions-Code-Repository.git
   ```

2. Open the R project:
   - Open `From-Forecasts-to-Decisions.Rproj` in RStudio

3. Run the analysis scripts in order:
   - `01_data_preparation.R` - Data loading and preprocessing
   - `02_exploratory_analysis.R` - Exploratory data analysis
   - `03_forecasting_models.R` - Build forecasting models
   - `04_staffing_decisions.R` - Staffing decision optimization
   - `05_performance_evaluation.R` - Evaluate model performance
   - `06_visualization.R` - Generate figures and plots

## Scripts Description

### 01_data_preparation.R
- Loads raw ED data
- Cleans and preprocesses the data
- Creates time series objects
- Handles missing values and outliers

### 02_exploratory_analysis.R
- Performs exploratory data analysis
- Generates descriptive statistics
- Analyzes temporal patterns and seasonality

### 03_forecasting_models.R
- Implements various forecasting methods:
  - ARIMA models
  - Exponential smoothing
  - Machine learning models (Random Forest, XGBoost)
  - Ensemble methods
- Tunes hyperparameters
- Saves trained models

### 04_staffing_decisions.R
- Implements staffing decision rules
- Calculates staffing requirements based on forecasts
- Applies cost-sensitive decision making

### 05_performance_evaluation.R
- Evaluates forecast accuracy metrics (MAE, RMSE, MAPE)
- Assesses operational performance metrics
- Compares different models and decision rules

### 06_visualization.R
- Creates publication-quality figures
- Generates comparison plots
- Produces result tables

## Data

Due to privacy considerations, the actual ED data is not included in this repository. The code is structured to work with properly formatted ED arrival data with the following columns:

- `datetime`: Timestamp of observation
- `arrivals`: Number of patient arrivals
- `staff_required`: Calculated staffing requirements
- Additional covariates as needed

Place your data files in the `data/` directory.

## Results

All results, including:
- Model performance metrics
- Staffing decision outcomes
- Comparative analyses

will be saved in the `results/` directory.

All figures and visualizations will be saved in the `figures/` directory.

## Citation

If you use this code in your research, please cite:

```
[Author names]. "From Forecasts to Decisions: Assessing the Operational Value 
of Forecasting Accuracy in Emergency Department Staffing Decisions." 
[Journal/Conference], [Year].
```

## License

This project is provided for research and educational purposes.

## Contact

For questions or issues, please open an issue on GitHub or contact the authors.

## Acknowledgments

This research was conducted as part of [institution/grant information].
