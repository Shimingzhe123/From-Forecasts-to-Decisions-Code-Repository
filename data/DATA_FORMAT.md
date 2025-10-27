# Data Structure Documentation

## Expected Data Format

The code is designed to work with Emergency Department (ED) arrival data with the following structure:

### Required Columns

1. **datetime** (POSIXct or character in ISO 8601 format)
   - Timestamp of each observation
   - Format: "YYYY-MM-DD HH:MM:SS"
   - Example: "2023-01-01 08:00:00"
   - Should be hourly observations (one row per hour)

2. **arrivals** (numeric)
   - Number of patient arrivals in that hour
   - Integer or numeric values
   - Example: 15 (representing 15 patients arrived in that hour)

### Optional Columns

Additional covariates can enhance model performance:

- **day_of_week**: Day of the week (1-7 or Mon-Sun)
- **is_holiday**: Binary indicator for holidays (0/1)
- **is_weekend**: Binary indicator for weekends (0/1)
- **weather_conditions**: Weather information if available
- **special_events**: Indicator for special events affecting ED arrivals
- **previous_arrivals**: Historical arrival counts (can be computed from data)

### Example Data Structure

```csv
datetime,arrivals
2023-01-01 00:00:00,12
2023-01-01 01:00:00,8
2023-01-01 02:00:00,6
2023-01-01 03:00:00,4
2023-01-01 04:00:00,5
2023-01-01 05:00:00,7
2023-01-01 06:00:00,10
2023-01-01 07:00:00,15
2023-01-01 08:00:00,20
...
```

### Data Requirements

- **Minimum duration**: At least 3 months of hourly data (recommended: 1+ years)
- **Completeness**: Minimal missing values; the preprocessing script handles some missing data
- **Frequency**: Hourly observations (24 observations per day)
- **Ordering**: Data should be in chronological order

### File Placement

Place your data file(s) in:
```
data/raw/ed_arrivals.csv
```

Or any filename of your choice in the `data/raw/` directory. Update the file path in `01_data_preparation.R` accordingly.

### Privacy Considerations

If your data contains sensitive patient information:
1. Ensure data is de-identified before uploading
2. Aggregate data to hourly counts (no individual records)
3. Consider adding this data directory to `.gitignore` if sharing code publicly
4. Follow your institution's data governance policies

### Data Quality Checks

The preprocessing script (01_data_preparation.R) will:
- Check for missing values
- Identify and handle outliers
- Ensure proper datetime formatting
- Validate data ranges
- Create derived features

If your data format differs from the expected structure, you may need to modify the data loading section in `01_data_preparation.R`.
