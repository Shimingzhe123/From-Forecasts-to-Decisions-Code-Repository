# ==============================================================================
# Paper 2 Full Reproducibility Pipeline with Additional Forecasting Models
#
# Manuscript:
#   The Operational Value of Forecasting: Bridging Statistical Accuracy and
#   Decision Utility in Healthcare Operations

# -----------------------------
# 0. User settings
# -----------------------------

DATA_DIR    <- "01data"
CACHE_DIR   <- "data"
RDS_DIR     <- "60rdsfiles"
PLOT_DIR    <- "plot"
PIC_DIR     <- "pic"

ARRIVAL_FILE <- file.path(DATA_DIR, "hospital2.csv")
HOLIDAY_FILE <- file.path(DATA_DIR, "holiday_rugby_all.xlsx")
SHIFT_FILE   <- file.path(DATA_DIR, "shiftschedule.xlsx")

# External/precomputed model files.
LSTM_RDS <- file.path(RDS_DIR, "68lstmforecastingresults.rds")
TIMEGPT_RDS <- file.path(CACHE_DIR, "timegpt_pred_cv.rds")
ADDITIONAL_FIRST_SET_RDS <- file.path(RDS_DIR, "fistsetofmodel.rds")

# Optional: if your actual file is named firstsetofmodel.rds rather than fistsetofmodel.rds,
# uncomment the next line.
# ADDITIONAL_FIRST_SET_RDS <- file.path(RDS_DIR, "firstsetofmodel.rds")

# TimeGPT is API-dependent. Default is to read precomputed RDS only.
RUN_TIMEGPT_API <- FALSE

# Rebuild forecasts by default so newly added models are included.
# If you already have a correct full cache, set this to FALSE.
FORCE_REBUILD_FORECASTS <- TRUE
POINT_FORECAST_CACHE <- file.path(CACHE_DIR, "point_forecast_results_all_models.rds")

# Main evaluation design.
EVAL_START <- lubridate::ymd_hms("2018-03-01 00:00:00", tz = "UTC")
TARGET_DAYS <- 365
SLOTS_PER_DAY <- 96
SLOT_MINUTES <- 15
FORECAST_HORIZON <- 96

# StaffReq mapping: 3.5h rolling window = 14 slots; 4:1 patient:nurse ratio.
WINDOW_SIZE_STAFFREQ <- 14
PATIENT_STAFF_RATIO <- 0.25
RESET_STAFFREQ_EACH_DAY <- TRUE

ME_DIRECTION <- "forecast_minus_actual"

set.seed(1)

# -----------------------------
# 1. Packages and folders
# -----------------------------

required_packages <- c(
  "tidyverse",
  "lubridate",
  "readr",
  "readxl",
  "tsibble",
  "fable",
  "fabletools",
  "feasts",
  "fable.prophet",
  "mgcv",
  "xgboost",
  "lightgbm",
  "lpSolve",
  "patchwork",
  "forcats",
  "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install missing packages before running this script:\n",
    paste(missing_packages, collapse = ", "),
    "\n\nExample:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
  library(readxl)
  library(tsibble)
  library(fable)
  library(fabletools)
  library(feasts)
  library(fable.prophet)
  library(mgcv)
  library(xgboost)
  library(lightgbm)
  library(lpSolve)
  library(patchwork)
  library(forcats)
  library(scales)
})

dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RDS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PIC_DIR, showWarnings = FALSE, recursive = TRUE)

msg <- function(...) cat(paste0(..., "\n"))

copy_to_pic <- function(path) {
  if (file.exists(path) && dir.exists(PIC_DIR)) {
    file.copy(path, file.path(PIC_DIR, basename(path)), overwrite = TRUE)
  }
  invisible(path)
}

# -----------------------------
# 2. Helper functions
# -----------------------------

rolling_sum <- function(x, window_size = WINDOW_SIZE_STAFFREQ) {
  x <- as.numeric(x)
  n <- length(x)
  
  if (n == 0) return(numeric(0))
  
  vapply(seq_len(n), function(i) {
    lo <- max(1, i - window_size + 1)
    sum(x[lo:i], na.rm = TRUE)
  }, numeric(1))
}

staff_req_from_arrivals <- function(arrivals,
                                    window_size = WINDOW_SIZE_STAFFREQ,
                                    ratio = PATIENT_STAFF_RATIO) {
  ceiling(rolling_sum(round(pmax(0, arrivals)), window_size = window_size) * ratio)
}

safe_nonnegative <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  pmax(0, x)
}

smape <- function(forecast, actual) {
  forecast <- as.numeric(forecast)
  actual <- as.numeric(actual)
  
  denominator <- (abs(forecast) + abs(actual)) / 2
  
  valid <- is.finite(forecast) &
    is.finite(actual) &
    is.finite(denominator) &
    denominator > 0
  
  if (!any(valid, na.rm = TRUE)) {
    return(NA_real_)
  }
  
  mean(
    abs(forecast[valid] - actual[valid]) / denominator[valid],
    na.rm = TRUE
  ) * 100
}

mean_error <- function(forecast, actual) {
  forecast <- as.numeric(forecast)
  actual <- as.numeric(actual)
  
  valid <- is.finite(forecast) & is.finite(actual)
  
  if (!any(valid, na.rm = TRUE)) {
    return(NA_real_)
  }
  
  if (ME_DIRECTION == "forecast_minus_actual") {
    mean(forecast[valid] - actual[valid], na.rm = TRUE)
  } else if (ME_DIRECTION == "actual_minus_forecast") {
    mean(actual[valid] - forecast[valid], na.rm = TRUE)
  } else {
    stop("Unknown ME_DIRECTION: ", ME_DIRECTION, call. = FALSE)
  }
}

safe_cor <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  ok <- complete.cases(x, y) & is.finite(x) & is.finite(y)
  
  if (sum(ok) < 3) return(NA_real_)
  
  sx <- sd(x[ok], na.rm = TRUE)
  sy <- sd(y[ok], na.rm = TRUE)
  
  if (is.na(sx) || is.na(sy) || sx == 0 || sy == 0) {
    return(NA_real_)
  }
  
  cor(x[ok], y[ok])
}

add_time_features <- function(df) {
  df %>%
    mutate(
      date = as.Date(time),
      slot = as.integer((hour(time) * 60 + minute(time)) / SLOT_MINUTES) + 1L,
      hour = hour(time),
      minute_quarter = case_when(
        minute(time) < 15 ~ 1L,
        minute(time) < 30 ~ 2L,
        minute(time) < 45 ~ 3L,
        TRUE ~ 4L
      ),
      wday_i = wday(time, week_start = 1),
      wday_f = factor(wday_i, levels = 1:7),
      hour_f = factor(hour, levels = 0:23),
      minute_quarter_f = factor(minute_quarter, levels = 1:4),
      doy = yday(time)
    )
}

align_matrix <- function(X_new, feature_names) {
  missing <- setdiff(feature_names, colnames(X_new))
  
  if (length(missing) > 0) {
    add <- matrix(0, nrow = nrow(X_new), ncol = length(missing))
    colnames(add) <- missing
    X_new <- cbind(X_new, add)
  }
  
  extra <- setdiff(colnames(X_new), feature_names)
  
  if (length(extra) > 0) {
    X_new <- X_new[, setdiff(colnames(X_new), extra), drop = FALSE]
  }
  
  X_new[, feature_names, drop = FALSE]
}

normalise_model_name <- function(x) {
  dplyr::recode(
    as.character(x),
    
    "snaivef" = "SNaive",
    "snaive" = "SNaive",
    "SNAIVE" = "SNaive",
    "SNaive" = "SNaive",
    
    "ets" = "ETS",
    "ETS" = "ETS",
    
    "prophet" = "Prophet",
    "Prophet" = "Prophet",
    
    "arima" = "ARIMA",
    "ARIMA" = "ARIMA",
    
    "gam" = "GAM",
    "GAM" = "GAM",
    
    "lgbm" = "LightGBM",
    "lightGBM" = "LightGBM",
    "LightGBM" = "LightGBM",
    
    "xgbm" = "XGBoost",
    "xgboost" = "XGBoost",
    "XGBoost" = "XGBoost",
    
    "mlr" = "MLR",
    "MLR" = "MLR",
    
    "theta" = "Theta",
    "Theta" = "Theta",
    
    "TimeGPT" = "TimeGPT",
    "timegpt" = "TimeGPT",
    
    "lstm" = "LSTM",
    "LSTM" = "LSTM",
    
    .default = as.character(x)
  )
}

normalise_forecast_tbl <- function(df, default_model = NULL) {
  nm <- names(df)
  
  pred_col <- c(
    "forecast_patients_arrival",
    "prediction",
    "gam_predict",
    "lgb_predict",
    "xgb_predict",
    ".mean",
    "TimeGPT",
    "LSTM",
    "forecast",
    "value"
  )
  pred_col <- pred_col[pred_col %in% nm][1]
  
  if (is.na(pred_col) || length(pred_col) == 0) {
    stop(
      "Cannot find prediction column in external forecast RDS. Columns are: ",
      paste(nm, collapse = ", "),
      call. = FALSE
    )
  }
  
  day_col <- c("day", "days", "origin", ".id")
  day_col <- day_col[day_col %in% nm][1]
  
  model_col <- c("model", ".model")
  model_col <- model_col[model_col %in% nm][1]
  
  actual_col <- c("actualnumber", "actual", "visit", "y")
  actual_col <- actual_col[actual_col %in% nm][1]
  
  if (!"time" %in% nm) {
    stop(
      "External forecast RDS must contain a time column. Columns are: ",
      paste(nm, collapse = ", "),
      call. = FALSE
    )
  }
  
  out <- df %>%
    as_tibble() %>%
    mutate(
      time = as.POSIXct(time, tz = "UTC"),
      forecast_patients_arrival = safe_nonnegative(.data[[pred_col]])
    )
  
  if (!is.na(day_col) && length(day_col) > 0) {
    out <- out %>% mutate(day = as.integer(.data[[day_col]]))
  } else {
    out <- out %>%
      mutate(day = as.integer(floor(as.numeric(difftime(time, EVAL_START, units = "days"))) + 1))
  }
  
  if (!is.na(model_col) && length(model_col) > 0) {
    out <- out %>% mutate(model = normalise_model_name(.data[[model_col]]))
  } else if (!is.null(default_model)) {
    out <- out %>% mutate(model = default_model)
  } else {
    stop("External forecast RDS has no model column and default_model is NULL.", call. = FALSE)
  }
  
  if (!is.na(actual_col) && length(actual_col) > 0) {
    out <- out %>% mutate(actualnumber = as.numeric(.data[[actual_col]]))
  } else {
    out <- out %>% mutate(actualnumber = NA_real_)
  }
  
  out %>%
    select(time, day, model, actualnumber, forecast_patients_arrival)
}

read_optional_forecast_rds <- function(path, default_model = NULL, label = basename(path)) {
  if (!file.exists(path)) {
    warning("Optional forecast RDS not found and will be skipped: ", path)
    return(tibble(
      time = as.POSIXct(character()),
      day = integer(),
      model = character(),
      actualnumber = numeric(),
      forecast_patients_arrival = numeric()
    ))
  }
  
  msg("Reading optional forecast RDS [", label, "]: ", path)
  
  normalise_forecast_tbl(
    readRDS(path),
    default_model = default_model
  )
}

# -----------------------------
# 3. Data preparation
# -----------------------------

prepare_subdaily_data <- function() {
  if (!file.exists(ARRIVAL_FILE)) {
    stop("Arrival file not found: ", ARRIVAL_FILE, call. = FALSE)
  }
  
  msg("Loading and aggregating ED arrivals: ", ARRIVAL_FILE)
  
  ae_original <- readr::read_csv(
    ARRIVAL_FILE,
    col_types = cols(arrival_time = col_datetime(format = "%d/%m/%Y %H:%M"))
  )
  
  if (!"arrival_time" %in% names(ae_original)) {
    stop("hospital2.csv must contain arrival_time.", call. = FALSE)
  }
  
  # Aggregate total ED arrivals per 15-minute interval.
  ae_subdaily <- ae_original %>%
    filter(!is.na(arrival_time)) %>%
    mutate(
      time = floor_date(arrival_time, "15 minutes"),
      time = force_tz(time, "UTC")
    ) %>%
    count(time, name = "visit") %>%
    as_tsibble(index = time) %>%
    fill_gaps(visit = 0) %>%
    as_tibble() %>%
    arrange(time) %>%
    add_time_features() %>%
    mutate(trend = row_number())
  
  if (file.exists(HOLIDAY_FILE)) {
    msg("Joining holiday/calendar variables: ", HOLIDAY_FILE)
    
    holidays_ae <- readxl::read_xlsx(HOLIDAY_FILE)
    
    if (!"Date" %in% names(holidays_ae)) {
      warning("Holiday file exists but has no Date column; holiday variables set to zero.")
      
      holiday_wales <- tibble(
        date = unique(ae_subdaily$date),
        is_public_holiday = 0L,
        is_school_holiday = 0L,
        xmas = 0L,
        new_year = 0L
      )
    } else {
      holiday_wales <- holidays_ae %>%
        mutate(
          date = as.Date(Date),
          xmas = if_else(mday(Date) == 25 & month(Date) == 12, 1L, 0L),
          new_year = if_else(mday(Date) == 1 & month(Date) == 1, 1L, 0L)
        ) %>%
        transmute(
          date,
          is_public_holiday = as.integer(replace_na(is_public_holiday, 0)),
          is_school_holiday = as.integer(replace_na(is_school_holiday, 0)),
          xmas,
          new_year
        ) %>%
        distinct(date, .keep_all = TRUE)
    }
  } else {
    warning("Holiday file not found; holiday variables set to zero: ", HOLIDAY_FILE)
    
    holiday_wales <- tibble(
      date = unique(ae_subdaily$date),
      is_public_holiday = 0L,
      is_school_holiday = 0L,
      xmas = 0L,
      new_year = 0L
    )
  }
  
  ae_subdaily <- ae_subdaily %>%
    left_join(holiday_wales, by = "date") %>%
    mutate(
      across(
        c(is_public_holiday, is_school_holiday, xmas, new_year),
        ~ replace_na(as.integer(.x), 0L)
      ),
      is_public_holiday_f = factor(is_public_holiday, levels = c(0, 1)),
      is_school_holiday_f = factor(is_school_holiday, levels = c(0, 1)),
      xmas_f = factor(xmas, levels = c(0, 1)),
      new_year_f = factor(new_year, levels = c(0, 1))
    ) %>%
    as_tsibble(index = time)
  
  readr::write_csv(as_tibble(ae_subdaily), file.path(CACHE_DIR, "ae_subdaily.csv"))
  saveRDS(ae_subdaily, file.path(RDS_DIR, "60ae_subdaily.rds"))
  
  ae_subdaily
}

make_eval_days <- function(ae_subdaily) {
  tibble(
    day = seq_len(TARGET_DAYS),
    day_start = EVAL_START + days(seq_len(TARGET_DAYS) - 1L),
    day_end = EVAL_START + days(seq_len(TARGET_DAYS)) - minutes(SLOT_MINUTES)
  ) %>%
    filter(day_end <= max(ae_subdaily$time))
}

# -----------------------------
# 4. Forecasting models
# -----------------------------

forecast_fable_models_one_day <- function(train_data, test_data) {
  train_ts <- train_data %>% as_tsibble(index = time)
  
  fit <- train_ts %>%
    model(
      SNaive = SNAIVE(sqrt(visit) ~ lag("week")),
      ETS = ETS(sqrt(visit) ~ error("A") + trend("A") + season("A")),
      ARIMA = ARIMA(visit),
      Theta = THETA(visit)
    )
  
  fit %>%
    forecast(new_data = test_data) %>%
    as_tibble() %>%
    transmute(
      time,
      model = normalise_model_name(.model),
      forecast_patients_arrival = safe_nonnegative(.mean)
    )
}

forecast_prophet_one_day <- function(train_data, test_data) {
  train_ts <- train_data %>% as_tsibble(index = time)
  
  fit <- train_ts %>%
    model(
      Prophet = prophet(
        sqrt(visit) ~
          season("day", type = "additive") +
          season("week", type = "additive") +
          season("year", type = "additive") +
          is_public_holiday +
          is_school_holiday +
          xmas +
          new_year +
          growth("linear")
      )
    )
  
  fit %>%
    forecast(new_data = test_data) %>%
    as_tibble() %>%
    transmute(
      time,
      model = "Prophet",
      forecast_patients_arrival = safe_nonnegative(.mean)
    )
}

forecast_mlr_one_day <- function(train_data, test_data) {
  train_df <- train_data %>% as_tibble()
  test_df <- test_data %>% as_tibble()
  
  fit <- lm(
    visit ~ trend + hour_f + minute_quarter_f + wday_f +
      is_public_holiday_f + is_school_holiday_f + xmas_f + new_year_f,
    data = train_df
  )
  
  tibble(
    time = test_df$time,
    model = "MLR",
    forecast_patients_arrival = safe_nonnegative(predict(fit, newdata = test_df))
  )
}

forecast_gam_one_day <- function(train_data, test_data) {
  train_df <- train_data %>% as_tibble()
  test_df <- test_data %>% as_tibble()
  
  fit <- mgcv::bam(
    visit ~ s(slot, bs = "cc", k = 20) +
      s(wday_i, bs = "re") +
      s(trend, k = 20) +
      is_public_holiday_f +
      is_school_holiday_f +
      xmas_f +
      new_year_f,
    data = train_df,
    family = gaussian(),
    knots = list(slot = c(1, SLOTS_PER_DAY))
  )
  
  tibble(
    time = test_df$time,
    model = "GAM",
    forecast_patients_arrival = safe_nonnegative(predict(fit, newdata = test_df))
  )
}

make_ml_matrix <- function(df, formula_obj = NULL, feature_names = NULL) {
  if (is.null(formula_obj)) {
    formula_obj <- ~ trend + slot + hour + minute_quarter + wday_f + doy +
      is_public_holiday_f + is_school_holiday_f + xmas_f + new_year_f
  }
  
  X <- model.matrix(formula_obj, data = as_tibble(df))
  
  if (!is.null(feature_names)) {
    X <- align_matrix(X, feature_names)
  }
  
  list(X = X, formula = formula_obj)
}

forecast_xgboost_one_day <- function(train_data, test_data) {
  train_df <- train_data %>% as_tibble()
  test_df <- test_data %>% as_tibble()
  
  mm_train <- make_ml_matrix(train_df)
  X_train <- mm_train$X
  y_train <- train_df$visit
  X_test <- make_ml_matrix(test_df, mm_train$formula, colnames(X_train))$X
  
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  
  params <- list(
    objective = "count:poisson",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    nthread = 1
  )
  
  fit <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  tibble(
    time = test_df$time,
    model = "XGBoost",
    forecast_patients_arrival = safe_nonnegative(predict(fit, newdata = X_test))
  )
}

forecast_lightgbm_one_day <- function(train_data, test_data) {
  train_df <- train_data %>% as_tibble()
  test_df <- test_data %>% as_tibble()
  
  mm_train <- make_ml_matrix(train_df)
  X_train <- mm_train$X
  y_train <- train_df$visit
  X_test <- make_ml_matrix(test_df, mm_train$formula, colnames(X_train))$X
  
  dtrain <- lgb.Dataset(data = X_train, label = y_train)
  
  params <- list(
    objective = "regression",
    metric = "l2",
    learning_rate = 0.05,
    num_leaves = 31,
    min_data_in_leaf = 20,
    feature_fraction = 0.9,
    bagging_fraction = 0.9,
    bagging_freq = 1,
    verbosity = -1,
    num_threads = 1
  )
  
  fit <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 300,
    verbose = -1
  )
  
  tibble(
    time = test_df$time,
    model = "LightGBM",
    forecast_patients_arrival = safe_nonnegative(predict(fit, X_test))
  )
}

forecast_timegpt_or_read <- function(ae_subdaily, eval_days) {
  if (!RUN_TIMEGPT_API) {
    if (file.exists(TIMEGPT_RDS)) {
      msg("Reading precomputed TimeGPT forecasts: ", TIMEGPT_RDS)
      return(normalise_forecast_tbl(readRDS(TIMEGPT_RDS), default_model = "TimeGPT"))
    }
    
    warning(
      "RUN_TIMEGPT_API is FALSE and TimeGPT RDS was not found. TimeGPT will be skipped: ",
      TIMEGPT_RDS
    )
    
    return(tibble(
      time = as.POSIXct(character()),
      day = integer(),
      model = character(),
      actualnumber = numeric(),
      forecast_patients_arrival = numeric()
    ))
  }
  
  if (!requireNamespace("nixtlar", quietly = TRUE)) {
    stop("RUN_TIMEGPT_API = TRUE but package 'nixtlar' is not installed.", call. = FALSE)
  }
  
  if (Sys.getenv("NIXTLA_API_KEY") == "") {
    stop("RUN_TIMEGPT_API = TRUE but NIXTLA_API_KEY is not set.", call. = FALSE)
  }
  
  if ("nixtla_set_api_key" %in% getNamespaceExports("nixtlar")) {
    nixtlar::nixtla_set_api_key(Sys.getenv("NIXTLA_API_KEY"))
  }
  
  out <- vector("list", nrow(eval_days))
  
  for (j in seq_len(nrow(eval_days))) {
    day <- eval_days$day[j]
    day_start <- eval_days$day_start[j]
    
    test_data <- ae_subdaily %>%
      filter(time >= day_start, time < day_start + days(1))
    
    train_data <- ae_subdaily %>%
      filter(time < day_start)
    
    hist <- train_data %>%
      transmute(unique_id = "ED", ds = time, y = visit)
    
    future_x <- test_data %>%
      transmute(unique_id = "ED", ds = time)
    
    fc <- nixtlar::nixtla_client_forecast(
      hist,
      h = FORECAST_HORIZON,
      X_df = future_x
    )
    
    out[[j]] <- fc %>%
      as_tibble() %>%
      rename(time = ds) %>%
      mutate(
        day = day,
        model = "TimeGPT",
        forecast_patients_arrival = safe_nonnegative(TimeGPT)
      ) %>%
      select(time, day, model, forecast_patients_arrival)
  }
  
  bind_rows(out)
}

read_lstm_forecasts <- function() {
  if (!file.exists(LSTM_RDS)) {
    warning("LSTM RDS not found. LSTM will be skipped: ", LSTM_RDS)
    
    return(tibble(
      time = as.POSIXct(character()),
      day = integer(),
      model = character(),
      actualnumber = numeric(),
      forecast_patients_arrival = numeric()
    ))
  }
  
  msg("Reading LSTM forecasts from RDS: ", LSTM_RDS)
  
  normalise_forecast_tbl(
    readRDS(LSTM_RDS),
    default_model = "LSTM"
  )
}

run_forecast_pipeline <- function(ae_subdaily, eval_days) {
  if (!FORCE_REBUILD_FORECASTS && file.exists(POINT_FORECAST_CACHE)) {
    msg("Reading cached point forecasts: ", POINT_FORECAST_CACHE)
    return(readRDS(POINT_FORECAST_CACHE))
  }
  
  msg("Generating rolling-origin forecasts for ", nrow(eval_days), " days.")
  
  main_out <- vector("list", nrow(eval_days))
  
  for (j in seq_len(nrow(eval_days))) {
    day <- eval_days$day[j]
    day_start <- eval_days$day_start[j]
    
    if (day %% 10 == 1 || day == nrow(eval_days)) {
      msg("Forecasting day ", day, " / ", nrow(eval_days))
    }
    
    train_data <- ae_subdaily %>%
      filter(time < day_start)
    
    test_data <- ae_subdaily %>%
      filter(time >= day_start, time < day_start + days(1))
    
    if (nrow(test_data) != SLOTS_PER_DAY) {
      warning(
        "Skipping day ", day,
        ": expected 96 test slots, got ", nrow(test_data)
      )
      next
    }
    
    parts <- list()
    
    parts$fable <- tryCatch(
      forecast_fable_models_one_day(train_data, test_data),
      error = function(e) {
        warning("Fable models failed on day ", day, ": ", e$message)
        NULL
      }
    )
    
    parts$prophet <- tryCatch(
      forecast_prophet_one_day(train_data, test_data),
      error = function(e) {
        warning("Prophet failed on day ", day, ": ", e$message)
        NULL
      }
    )
    
    parts$mlr <- tryCatch(
      forecast_mlr_one_day(train_data, test_data),
      error = function(e) {
        warning("MLR failed on day ", day, ": ", e$message)
        NULL
      }
    )
    
    parts$gam <- tryCatch(
      forecast_gam_one_day(train_data, test_data),
      error = function(e) {
        warning("GAM failed on day ", day, ": ", e$message)
        NULL
      }
    )
    
    parts$xgb <- tryCatch(
      forecast_xgboost_one_day(train_data, test_data),
      error = function(e) {
        warning("XGBoost failed on day ", day, ": ", e$message)
        NULL
      }
    )
    
    parts$lgb <- tryCatch(
      forecast_lightgbm_one_day(train_data, test_data),
      error = function(e) {
        warning("LightGBM failed on day ", day, ": ", e$message)
        NULL
      }
    )
    
    main_out[[j]] <- bind_rows(parts) %>%
      mutate(day = day) %>%
      left_join(
        test_data %>%
          as_tibble() %>%
          select(time, actualnumber = visit),
        by = "time"
      ) %>%
      select(time, day, model, actualnumber, forecast_patients_arrival)
  }
  
  point_forecasts_main <- bind_rows(main_out)
  
  # External/precomputed forecasts.
  timegpt_fc <- forecast_timegpt_or_read(ae_subdaily, eval_days)
  lstm_fc <- read_lstm_forecasts()
  
  # Optional first-set file: often contains Prophet, ETS, SNaive and LSTM.
  first_set_fc <- read_optional_forecast_rds(
    ADDITIONAL_FIRST_SET_RDS,
    default_model = NULL,
    label = "additional first-set models"
  )
  
  actual_lookup <- ae_subdaily %>%
    as_tibble() %>%
    filter(time >= min(eval_days$day_start), time <= max(eval_days$day_end)) %>%
    select(time, actual_lookup = visit)
  
  external_fc <- bind_rows(timegpt_fc, lstm_fc, first_set_fc) %>%
    mutate(model = normalise_model_name(model)) %>%
    left_join(actual_lookup, by = "time") %>%
    mutate(actualnumber = coalesce(actualnumber, actual_lookup)) %>%
    select(time, day, model, actualnumber, forecast_patients_arrival)
  
  point_forecasts <- bind_rows(point_forecasts_main, external_fc) %>%
    filter(day >= 1, day <= TARGET_DAYS) %>%
    mutate(
      model = normalise_model_name(model),
      time = as.POSIXct(time, tz = "UTC"),
      day = as.integer(day),
      forecast_patients_arrival = safe_nonnegative(forecast_patients_arrival),
      actualnumber = as.numeric(actualnumber)
    ) %>%
    arrange(model, day, time) %>%
    distinct(model, day, time, .keep_all = TRUE)
  
  model_day_quality <- point_forecasts %>%
    group_by(model, day) %>%
    summarise(
      n_slots = n(),
      n_unique_time = n_distinct(time),
      missing_actual = sum(!is.finite(actualnumber) | is.na(actualnumber)),
      missing_forecast = sum(!is.finite(forecast_patients_arrival) | is.na(forecast_patients_arrival)),
      complete_model_day =
        n_slots == SLOTS_PER_DAY &&
        n_unique_time == SLOTS_PER_DAY &&
        missing_actual == 0 &&
        missing_forecast == 0,
      .groups = "drop"
    )
  
  readr::write_csv(
    model_day_quality,
    file.path(CACHE_DIR, "point_forecast_model_day_quality.csv")
  )
  
  msg("Forecast model-day quality summary:")
  print(
    model_day_quality %>%
      group_by(model) %>%
      summarise(
        total_days = n(),
        complete_days = sum(complete_model_day),
        incomplete_days = sum(!complete_model_day),
        min_slots = min(n_slots),
        max_slots = max(n_slots),
        total_missing_actual = sum(missing_actual),
        total_missing_forecast = sum(missing_forecast),
        .groups = "drop"
      ),
    n = Inf
  )
  
  # Keep the forecast-level table complete enough for downstream optimisation.
  complete_model_days <- model_day_quality %>%
    filter(complete_model_day) %>%
    select(model, day)
  
  point_forecasts <- point_forecasts %>%
    inner_join(complete_model_days, by = c("model", "day")) %>%
    arrange(model, day, time)
  
  saveRDS(point_forecasts, POINT_FORECAST_CACHE)
  readr::write_csv(point_forecasts, file.path(CACHE_DIR, "point_forecast_results_all_models.csv"))
  
  # Compatibility alias for your older workflow.
  saveRDS(point_forecasts, file.path(CACHE_DIR, "pointforecast_staffdemand.rds"))
  
  point_forecasts
}

# -----------------------------
# 5. Forecast accuracy and StaffReq conversion
# -----------------------------

compute_accuracy_daily <- function(point_forecast_results) {
  point_forecast_results %>%
    group_by(day, model) %>%
    summarise(
      ME = mean_error(forecast_patients_arrival, actualnumber),
      
      RMSE = {
        err <- forecast_patients_arrival - actualnumber
        sqrt(mean(err^2, na.rm = TRUE))
      },
      
      MAE = {
        err <- forecast_patients_arrival - actualnumber
        mean(abs(err), na.rm = TRUE)
      },
      
      sMAPE = smape(forecast_patients_arrival, actualnumber),
      
      MASE = {
        err <- forecast_patients_arrival - actualnumber
        denom <- mean(abs(diff(actualnumber)), na.rm = TRUE)
        
        ifelse(
          is.na(denom) || denom == 0,
          NA_real_,
          mean(abs(err), na.rm = TRUE) / denom
        )
      },
      
      .groups = "drop"
    )
}

compute_accuracy_summary <- function(accuracy_daily) {
  accuracy_daily %>%
    group_by(model) %>%
    summarise(
      ME = mean(ME, na.rm = TRUE),
      RMSE = sqrt(mean(RMSE^2, na.rm = TRUE)),
      MAE = mean(MAE, na.rm = TRUE),
      sMAPE = mean(sMAPE, na.rm = TRUE),
      MASE = mean(MASE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(RMSE)
}

convert_to_staff_demand <- function(point_forecast_results) {
  grouping <- if (RESET_STAFFREQ_EACH_DAY) c("model", "day") else c("model")
  
  point_forecast_results %>%
    arrange(model, day, time) %>%
    group_by(across(all_of(grouping))) %>%
    mutate(
      cumulative_patient_actual = rolling_sum(actualnumber),
      cumulative_patient_forecast = rolling_sum(forecast_patients_arrival),
      cumulative_staff_actual = ceiling(cumulative_patient_actual * PATIENT_STAFF_RATIO),
      cumulative_staff_forecast = ceiling(cumulative_patient_forecast * PATIENT_STAFF_RATIO)
    ) %>%
    ungroup() %>%
    select(
      time,
      day,
      model,
      actualnumber,
      forecast_patients_arrival,
      cumulative_staff_actual,
      cumulative_staff_forecast
    )
}

# -----------------------------
# 6. Deterministic set-covering staffing optimisation
# -----------------------------

load_shift_matrix <- function() {
  if (!file.exists(SHIFT_FILE)) {
    stop("Shift schedule file not found: ", SHIFT_FILE, call. = FALSE)
  }
  
  shiftpattern <- readxl::read_xlsx(SHIFT_FILE)
  
  shift_cols <- grep("^Shift", names(shiftpattern), value = TRUE)
  
  if (length(shift_cols) == 0) {
    stop(
      "No columns starting with 'Shift' found in ",
      SHIFT_FILE,
      call. = FALSE
    )
  }
  
  f.con <- shiftpattern %>%
    select(all_of(shift_cols)) %>%
    as.matrix()
  
  storage.mode(f.con) <- "numeric"
  
  if (nrow(f.con) != SLOTS_PER_DAY && ncol(f.con) == SLOTS_PER_DAY) {
    f.con <- t(f.con)
  }
  
  if (nrow(f.con) != SLOTS_PER_DAY) {
    stop(
      "Shift coverage matrix must have 96 rows after orientation check. Current dimensions: ",
      paste(dim(f.con), collapse = " x "),
      call. = FALSE
    )
  }
  
  colnames(f.con) <- paste0("shift", seq_len(ncol(f.con)))
  
  f.con
}

solve_staffing_lp <- function(req_vec, f.con) {
  req_vec <- as.numeric(req_vec)
  req_vec[is.na(req_vec)] <- 0
  req_vec <- ceiling(pmax(0, req_vec))
  
  if (length(req_vec) != nrow(f.con)) {
    stop(
      "Requirement vector length must equal number of slots. Got ",
      length(req_vec),
      " expected ",
      nrow(f.con),
      call. = FALSE
    )
  }
  
  n_shifts <- ncol(f.con)
  
  sol <- lpSolve::lp(
    direction = "min",
    objective.in = rep(1, n_shifts),
    const.mat = f.con,
    const.dir = rep(">=", nrow(f.con)),
    const.rhs = req_vec,
    int.vec = seq_len(n_shifts)
  )
  
  if (sol$status != 0) {
    warning("lpSolve did not return optimal status. Status = ", sol$status)
  }
  
  as.numeric(sol$solution)
}

run_staffing_optimisation <- function(pointforecast_staffdemand, f.con) {
  msg("Running deterministic set-covering optimisation.")
  
  shift_names <- colnames(f.con)
  shift_hours <- colSums(f.con) * (SLOT_MINUTES / 60)
  
  model_day_groups <- pointforecast_staffdemand %>%
    arrange(model, day, time) %>%
    group_by(model, day) %>%
    summarise(
      n_slots = n(),
      n_unique_time = n_distinct(time),
      has_missing_req = any(
        !is.finite(cumulative_staff_forecast) |
          !is.finite(cumulative_staff_actual)
      ),
      forecast_req = list(cumulative_staff_forecast),
      actual_req = list(cumulative_staff_actual),
      .groups = "drop"
    ) %>%
    filter(
      n_slots == SLOTS_PER_DAY,
      n_unique_time == SLOTS_PER_DAY,
      !has_missing_req
    )
  
  forecast_solutions <- vector("list", nrow(model_day_groups))
  perfect_solutions <- vector("list", nrow(model_day_groups))
  
  for (i in seq_len(nrow(model_day_groups))) {
    if (i %% 500 == 1 || i == nrow(model_day_groups)) {
      msg("LP model-day ", i, " / ", nrow(model_day_groups))
    }
    
    x_forecast <- solve_staffing_lp(model_day_groups$forecast_req[[i]], f.con)
    x_perfect <- solve_staffing_lp(model_day_groups$actual_req[[i]], f.con)
    
    forecast_solutions[[i]] <- tibble(
      model = model_day_groups$model[i],
      day = model_day_groups$day[i],
      shift = shift_names,
      staffforecastoptimize = x_forecast
    )
    
    perfect_solutions[[i]] <- tibble(
      model = model_day_groups$model[i],
      day = model_day_groups$day[i],
      shift = shift_names,
      Perfect_information = x_perfect
    )
  }
  
  forecast_long <- bind_rows(forecast_solutions)
  perfect_long <- bind_rows(perfect_solutions)
  
  staff_demand_all_information <- forecast_long %>%
    left_join(perfect_long, by = c("model", "day", "shift")) %>%
    mutate(
      shift_hours = shift_hours[match(shift, shift_names)],
      forecast_staff_hours = staffforecastoptimize * shift_hours,
      perfect_staff_hours = Perfect_information * shift_hours
    )
  
  saveRDS(perfect_long, file.path(CACHE_DIR, "perfectinformation.rds"))
  saveRDS(staff_demand_all_information, file.path(CACHE_DIR, "staff_demand_all_information.rds"))
  
  summary_day <- staff_demand_all_information %>%
    group_by(model, day) %>%
    summarise(
      sum_staffnum_forecast_daily = sum(staffforecastoptimize, na.rm = TRUE),
      sum_staffnum_perfect_daily = sum(Perfect_information, na.rm = TRUE),
      forecast_staff_hours = sum(forecast_staff_hours, na.rm = TRUE),
      perfect_staff_hours = sum(perfect_staff_hours, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      date = as.Date(EVAL_START) + days(day - 1),
      differencestaffnum = sum_staffnum_forecast_daily - sum_staffnum_perfect_daily,
      abs_differencestaffnum = abs(differencestaffnum),
      staffing_hours_difference = forecast_staff_hours - perfect_staff_hours,
      abs_staffing_hours_difference = abs(staffing_hours_difference)
    ) %>%
    select(date, day, model, everything())
  
  saveRDS(summary_day, file.path(CACHE_DIR, "71daysummaryallimpact.rds"))
  readr::write_csv(summary_day, file.path(CACHE_DIR, "71daysummaryallimpact.csv"))
  
  list(
    forecast_long = forecast_long,
    perfect_long = perfect_long,
    staff_demand_all_information = staff_demand_all_information,
    summary_day = summary_day
  )
}

# -----------------------------
# 7. Result tables
# -----------------------------

make_result_tables <- function(point_forecast_results, pointforecast_staffdemand, summary_day) {
  accuracy_daily <- compute_accuracy_daily(point_forecast_results)
  accuracy_model_summary <- compute_accuracy_summary(accuracy_daily)
  
  accuracy_summary <- summary_day %>%
    select(
      model,
      day,
      date,
      sum_staffnum_forecast_daily,
      sum_staffnum_perfect_daily,
      forecast_staff_hours,
      perfect_staff_hours,
      differencestaffnum,
      abs_differencestaffnum,
      staffing_hours_difference,
      abs_staffing_hours_difference
    ) %>%
    left_join(accuracy_daily, by = c("model", "day")) %>%
    arrange(model, day)
  
  operational_summary <- accuracy_summary %>%
    group_by(model) %>%
    summarise(
      Total_Forecasted_Staffing = sum(sum_staffnum_forecast_daily, na.rm = TRUE),
      Total_Perfect_Staffing = sum(sum_staffnum_perfect_daily, na.rm = TRUE),
      Total_Staffing_Deviation = abs(Total_Forecasted_Staffing - Total_Perfect_Staffing),
      Total_Daily_Staffing_Imbalance = sum(abs_differencestaffnum, na.rm = TRUE),
      Avg_Abs_Daily_Staffing_Imbalance = mean(abs_differencestaffnum, na.rm = TRUE),
      Avg_Abs_Staffing_Hours_Imbalance = mean(abs_staffing_hours_difference, na.rm = TRUE),
      Overstaffing_Days = sum(differencestaffnum > 0, na.rm = TRUE),
      Understaffing_Days = sum(differencestaffnum < 0, na.rm = TRUE),
      Balanced_Days = sum(differencestaffnum == 0, na.rm = TRUE),
      Imbalance_Variance = var(differencestaffnum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Avg_Abs_Daily_Staffing_Imbalance)
  
  correlation_results <- accuracy_summary %>%
    group_by(model) %>%
    summarise(
      ME = safe_cor(differencestaffnum, ME),
      RMSE = safe_cor(differencestaffnum, RMSE),
      MAE = safe_cor(differencestaffnum, MAE),
      MASE = safe_cor(differencestaffnum, MASE),
      .groups = "drop"
    )
  
  saveRDS(accuracy_daily, file.path(CACHE_DIR, "accuracy_daily.rds"))
  saveRDS(accuracy_model_summary, file.path(CACHE_DIR, "accuracy_model_summary.rds"))
  saveRDS(accuracy_summary, file.path(CACHE_DIR, "74accuracy_summary.rds"))
  saveRDS(operational_summary, file.path(CACHE_DIR, "model_operational_summary.rds"))
  saveRDS(correlation_results, file.path(CACHE_DIR, "correlation_results.rds"))
  
  write_csv(accuracy_daily, file.path(CACHE_DIR, "accuracy_daily.csv"))
  write_csv(accuracy_model_summary, file.path(CACHE_DIR, "model_accuracy_summary.csv"))
  write_csv(accuracy_summary, file.path(CACHE_DIR, "overall_accuracy_summary.csv"))
  write_csv(operational_summary, file.path(CACHE_DIR, "model_operational_summary.csv"))
  write_csv(correlation_results, file.path(CACHE_DIR, "correlation_results.csv"))
  
  list(
    accuracy_daily = accuracy_daily,
    accuracy_model_summary = accuracy_model_summary,
    accuracy_summary = accuracy_summary,
    operational_summary = operational_summary,
    correlation_results = correlation_results
  )
}

# -----------------------------
# 8. Manuscript plots
# -----------------------------

plot_manuscript_figures <- function(tables) {
  accuracy_summary <- tables$accuracy_summary
  operational_summary <- tables$operational_summary
  correlation_results <- tables$correlation_results
  
  model_order <- operational_summary %>%
    arrange(Avg_Abs_Daily_Staffing_Imbalance) %>%
    pull(model)
  
  accuracy_summary <- accuracy_summary %>%
    mutate(model = factor(model, levels = model_order))
  
  operational_summary <- operational_summary %>%
    mutate(model = factor(model, levels = model_order))
  
  p_avg <- operational_summary %>%
    ggplot(
      aes(
        y = fct_reorder(model, Avg_Abs_Daily_Staffing_Imbalance),
        x = Avg_Abs_Daily_Staffing_Imbalance
      )
    ) +
    geom_segment(
      aes(
        x = 0,
        xend = Avg_Abs_Daily_Staffing_Imbalance,
        yend = model
      ),
      linetype = "dotted",
      color = "grey50"
    ) +
    geom_point(size = 3) +
    labs(
      x = "Average Absolute Daily Staffing Imbalance",
      y = "Forecasting Models"
    ) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  path <- file.path(PLOT_DIR, "Avg_staffing_plot.png")
  ggsave(path, p_avg, width = 6.5, height = 4.5, units = "in", dpi = 600)
  copy_to_pic(path)
  
  perfect_ref <- operational_summary %>%
    summarise(x = mean(Total_Perfect_Staffing, na.rm = TRUE)) %>%
    pull(x)
  
  sum_by_model_prepped <- operational_summary %>%
    transmute(
      model,
      `Total Forecasted Staffing` = Total_Forecasted_Staffing,
      `Staffing Imbalance` = Total_Daily_Staffing_Imbalance
    ) %>%
    pivot_longer(
      -model,
      names_to = "Types",
      values_to = "Counts"
    ) %>%
    mutate(
      Reference = if_else(Types == "Total Forecasted Staffing", perfect_ref, 0),
      Difference = abs(Counts - Reference)
    )
  
  p_total <- sum_by_model_prepped %>%
    group_by(Types) %>%
    mutate(model_ordered = fct_reorder(model, Difference, .desc = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(y = model_ordered, x = Counts)) +
    geom_segment(
      aes(x = 0, xend = Counts, yend = model_ordered),
      color = "grey50",
      linetype = "dotted"
    ) +
    geom_point(size = 3) +
    geom_vline(
      aes(xintercept = Reference),
      color = "red",
      linetype = "dashed"
    ) +
    facet_grid(Types ~ ., scales = "free_x") +
    labs(
      x = "Staffing Counts",
      y = "Forecasting Models"
    ) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  path <- file.path(PLOT_DIR, "forecast_evaplot.png")
  ggsave(path, p_total, width = 7.2, height = 5.2, units = "in", dpi = 600)
  copy_to_pic(path)
  
  over_under <- operational_summary %>%
    select(model, Overstaffing_Days, Understaffing_Days) %>%
    pivot_longer(
      c(Overstaffing_Days, Understaffing_Days),
      names_to = "Types",
      values_to = "Counts"
    ) %>%
    mutate(
      Types = recode(
        Types,
        Overstaffing_Days = "Overstaffing Days",
        Understaffing_Days = "Understaffing Days"
      )
    )
  
  p_over_under <- over_under %>%
    group_by(Types) %>%
    mutate(model_ordered = fct_reorder(model, Counts, .desc = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(y = model_ordered, x = Counts)) +
    geom_segment(
      aes(x = 0, xend = Counts, yend = model_ordered),
      color = "grey50",
      linetype = "dotted"
    ) +
    geom_point(size = 3) +
    facet_grid(Types ~ ., scales = "free_x") +
    labs(
      x = "Number of Days",
      y = "Forecasting Models"
    ) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  path <- file.path(PLOT_DIR, "forecast_evaplot_over_understaffing.png")
  ggsave(path, p_over_under, width = 7.2, height = 5.2, units = "in", dpi = 600)
  copy_to_pic(path)
  
  metric_long <- accuracy_summary %>%
    select(model, day, differencestaffnum, ME, RMSE, MAE, sMAPE, MASE) %>%
    pivot_longer(
      c(ME, RMSE, MAE, sMAPE, MASE),
      names_to = "Metric",
      values_to = "Value"
    )
  
  p_scatter <- metric_long %>%
    ggplot(aes(x = Value, y = differencestaffnum, shape = model)) +
    geom_point(size = 1.5, alpha = 0.45) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    facet_wrap(~ Metric, scales = "free_x", ncol = 2) +
    labs(
      x = "Daily forecast error metric",
      y = "Daily Staffing Imbalance Count"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  path <- file.path(PLOT_DIR, "newmodelaccuracy_summary.png")
  ggsave(path, p_scatter, width = 12, height = 8, units = "in", dpi = 300)
  copy_to_pic(path)
  
  corr_long <- correlation_results %>%
    pivot_longer(
      -model,
      names_to = "Metric",
      values_to = "Correlation"
    )
  
  p_heat <- corr_long %>%
    ggplot(
      aes(
        x = Metric,
        y = fct_reorder(model, abs(Correlation), .fun = max),
        fill = Correlation
      )
    ) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", Correlation)), size = 3) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    labs(
      x = "Forecast error metric",
      y = "Forecasting Model",
      fill = "Correlation"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  path <- file.path(PLOT_DIR, "correlation_heatmap.png")
  ggsave(path, p_heat, width = 7.5, height = 5.0, units = "in", dpi = 600)
  copy_to_pic(path)
  
  invisible(TRUE)
}

# -----------------------------
# 9. Main execution
# -----------------------------

msg("=== Starting full Paper 2 pipeline with added models ===")

ae_subdaily <- prepare_subdaily_data()
eval_days <- make_eval_days(ae_subdaily)

if (nrow(eval_days) < TARGET_DAYS) {
  warning(
    "Only ",
    nrow(eval_days),
    " complete evaluation days are available from ",
    as.character(EVAL_START),
    "."
  )
}

point_forecast_results <- run_forecast_pipeline(ae_subdaily, eval_days)

msg("Models included in point_forecast_results:")
print(sort(unique(point_forecast_results$model)))

msg("Rows in point_forecast_results: ", nrow(point_forecast_results))

pointforecast_staffdemand <- convert_to_staff_demand(point_forecast_results)

saveRDS(
  pointforecast_staffdemand,
  file.path(CACHE_DIR, "staff_demand_from_forecasts.rds")
)

write_csv(
  pointforecast_staffdemand,
  file.path(CACHE_DIR, "staff_demand_from_forecasts.csv")
)

f.con <- load_shift_matrix()

opt_results <- run_staffing_optimisation(
  pointforecast_staffdemand = pointforecast_staffdemand,
  f.con = f.con
)

tables <- make_result_tables(
  point_forecast_results = point_forecast_results,
  pointforecast_staffdemand = pointforecast_staffdemand,
  summary_day = opt_results$summary_day
)

plot_manuscript_figures(tables)

msg("=== Done ===")
msg("Key outputs:")
msg("  ", file.path(CACHE_DIR, "point_forecast_results_all_models.rds"))
msg("  ", file.path(CACHE_DIR, "pointforecast_staffdemand.rds"))
msg("  ", file.path(CACHE_DIR, "staff_demand_from_forecasts.rds"))
msg("  ", file.path(CACHE_DIR, "model_accuracy_summary.csv"))
msg("  ", file.path(CACHE_DIR, "model_operational_summary.csv"))
msg("  ", file.path(CACHE_DIR, "overall_accuracy_summary.csv"))
msg("  ", file.path(CACHE_DIR, "correlation_results.csv"))
msg("  ", PLOT_DIR, "/ and ", PIC_DIR, "/ figures")
