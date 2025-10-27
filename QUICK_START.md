# Quick Start Guide

## Getting Started with the From Forecasts to Decisions Code Repository

This guide will help you get started with using the R code for analyzing ED staffing decisions.

### Prerequisites

- **R** (version 4.0 or higher recommended)
- **RStudio** (recommended but not required)
- Your ED arrival data in CSV format

### Step 1: Install Required Packages

Open R or RStudio and run the package installation script:

```r
source("install_packages.R")
```

This will automatically install all required packages. The installation may take several minutes.

### Step 2: Prepare Your Data

1. Place your ED arrival data in the `data/raw/` directory
2. Ensure your data follows the format described in `data/DATA_FORMAT.md`
3. The minimum required columns are:
   - `datetime`: Timestamp for each observation
   - `arrivals`: Number of patient arrivals

Example data format:
```csv
datetime,arrivals
2023-01-01 00:00:00,12
2023-01-01 01:00:00,8
...
```

### Step 3: Run the Analysis Scripts in Order

Open the R project by double-clicking `From-Forecasts-to-Decisions.Rproj` in RStudio.

Then run the scripts in this order:

#### 1. Data Preparation
```r
source("scripts/01_data_preparation.R")
```
- Loads and cleans your data
- Creates features for modeling
- Splits data into training and test sets

#### 2. Exploratory Analysis
```r
source("scripts/02_exploratory_analysis.R")
```
- Generates descriptive statistics
- Creates visualizations of patterns
- Analyzes seasonality and trends

#### 3. Forecasting Models
```r
source("scripts/03_forecasting_models.R")
```
- Trains multiple forecasting models (ARIMA, ETS, Random Forest, XGBoost)
- Generates predictions
- Saves trained models

#### 4. Staffing Decisions
```r
source("scripts/04_staffing_decisions.R")
```
- Implements different staffing decision rules
- Evaluates staffing performance
- Calculates operational costs

#### 5. Performance Evaluation
```r
source("scripts/05_performance_evaluation.R")
```
- Compares forecast accuracy across models
- Evaluates operational performance
- Analyzes forecast-decision relationships

#### 6. Visualization
```r
source("scripts/06_visualization.R")
```
- Creates publication-quality figures
- Generates comparison plots
- Visualizes results

### Step 4: Review Results

After running all scripts, check:
- `results/` - CSV files with metrics and comparisons
- `figures/` - PNG files with visualizations
- `models/` - Saved model objects for future use

### Customization

#### Adjusting Staffing Parameters

Edit the `STAFFING_PARAMS` in `scripts/04_staffing_decisions.R`:

```r
STAFFING_PARAMS <- list(
  patients_per_nurse = 4,      # Adjust based on your ED
  patients_per_doctor = 8,     # Adjust based on your ED
  cost_understaffing = 100,    # Your cost estimates
  cost_overstaffing = 50,      # Your cost estimates
  ...
)
```

#### Selecting Different Models

In `scripts/03_forecasting_models.R`, you can:
- Add or remove forecasting models
- Adjust hyperparameters
- Change the train-test split ratio

#### Modifying Visualizations

In `scripts/06_visualization.R`, customize:
- Plot themes and colors
- Figure dimensions
- Which metrics to visualize

### Troubleshooting

**Problem**: Script fails with "file not found" error
- **Solution**: Make sure you're in the project directory and have run previous scripts

**Problem**: Missing packages error
- **Solution**: Run `source("install_packages.R")` again

**Problem**: Data loading fails
- **Solution**: Check that your data file path is correct and matches the format in `data/DATA_FORMAT.md`

**Problem**: Out of memory errors
- **Solution**: Consider using a subset of your data or increasing R's memory limit

### Running Individual Sections

Each script is organized into numbered sections. You can run individual sections by:
1. **Ensure you're in the project root directory**: Either open the .Rproj file in RStudio or set the working directory with `setwd("path/to/project")`
2. Opening the script in RStudio
3. Highlighting the section you want to run
4. Pressing Ctrl+Enter (Windows/Linux) or Cmd+Enter (Mac)

**Important**: When running individual scripts outside of RStudio, make sure your working directory is set to the project root before sourcing the script:
```r
setwd("/path/to/From-Forecasts-to-Decisions-Code-Repository")
source("scripts/01_data_preparation.R")
```

### Need Help?

- Review the comments in each script for detailed explanations
- Check `data/DATA_FORMAT.md` for data requirements
- Refer to the main `README.md` for an overview
- Open an issue on GitHub for specific problems

### Next Steps

After completing the analysis:
1. Review the accuracy metrics to select the best model
2. Analyze the staffing decision comparisons
3. Use the visualizations in your paper or presentations
4. Consider running sensitivity analyses with different parameters
5. Document any modifications you make for reproducibility

## Tips for Best Results

- Use at least 1 year of hourly data for reliable models
- Ensure data quality (minimal missing values, no outliers)
- Run exploratory analysis first to understand your data
- Compare multiple models and decision rules
- Adjust staffing parameters to match your ED context
- Save your results and models for future reference

Happy analyzing!
