# ==============================================================================
# Reproducibility script for:
# The Operational Value of Forecasting: Bridging Statistical Accuracy and
# Decision Utility in Healthcare Operations
#
# Purpose
#   End-to-end reproduction pipeline for the point-forecast + deterministic
#   staffing optimisation manuscript:
#     1. Load and aggregate ED arrivals to 15-minute intervals
#     2. Build rolling-origin one-day-ahead forecasts over 365 days
#     3. Keep LSTM as an external/precomputed RDS input, as requested
#     4. Convert forecasts to nurse requirements using 3.5h rolling window
#     5. Solve deterministic set-covering staffing models with lpSolve
#     6. Compute perfect-information benchmark, daily staffing imbalance,
#        overstaffing/understaffing days, accuracy tables and correlation tables
#     7. Export manuscript-ready data tables and plots
#
# IMPORTANT manuscript alignment note
#   This code defines ME as forecast - actual, consistent with the original
#   analysis scripts and with the sign of daily staffing imbalance:
#       DSI = forecast-based staffing - perfect-information staffing
#   Therefore positive ME means over-forecasting and usually positive DSI means
#   over-staffing. If the manuscript currently defines ME as actual - forecast,
#   revise the manuscript formula to match this code, or change ME_DIRECTION
#   below and regenerate all correlation results.
# ==============================================================================

# -----------------------------
# 0. User settings
# -----------------------------
DATA_DIR    <- "01data"
CACHE_DIR   <- "data"
RDS_DIR     <- "60rdsfiles"
PLOT_DIR    <- "plot"
PIC_DIR     <- "pic"       # Overleaf graphicspath in the manuscript

ARRIVAL_FILE <- file.path(DATA_DIR, "hospital2.csv")
HOLIDAY_FILE <- file.path(DATA_DIR, "holiday_rugby_all.xlsx")
SHIFT_FILE   <- file.path(DATA_DIR, "shiftschedule.xlsx")

# LSTM remains read from RDS, as requested.
LSTM_RDS <- file.path(RDS_DIR, "68lstmforecastingresults.rds")

# TimeGPT is API-dependent. For reproducibility without exposing API keys, the
# default is to read a precomputed RDS if available. Set RUN_TIMEGPT_API = TRUE
# only if nixtlar is configured locally and NIXTLA_API_KEY is available.
TIMEGPT_RDS <- file.path(CACHE_DIR, "timegpt_pred_cv.rds")
RUN_TIMEGPT_API <- FALSE

# Main evaluation design in the manuscript.
EVAL_START <- lubridate::ymd_hms("2018-03-01 00:00:00", tz = "UTC")
TARGET_DAYS <- 365
SLOTS_PER_DAY <- 96
SLOT_MINUTES <- 15
FORECAST_HORIZON <- 96

# StaffReq mapping: 3.5h rolling window = 14 slots; 4:1 patient:nurse ratio.
WINDOW_SIZE_STAFFREQ <- 14
PATIENT_STAFF_RATIO <- 0.25

# Use TRUE to align the StaffReq mapping with daily 96-step planning horizons.
# Set FALSE only if you intentionally want the legacy across-year rolling sum.
RESET_STAFFREQ_EACH_DAY <- TRUE

# Mean-error direction.
# Recommended for this paper: forecast_minus_actual.
ME_DIRECTION <- "forecast_minus_actual"  # choices: "forecast_minus_actual", "actual_minus_forecast"

# Forecast runtime settings.
FORCE_REBUILD_FORECASTS <- FALSE
POINT_FORECAST_CACHE <- file.path(CACHE_DIR, "point_forecast_results_all_models.rds")

# Parallelisation is intentionally not forced here to keep the script portable.
# Heavy models are run sequentially by default. You may parallelise later if needed.
set.seed(1)

# -----------------------------
# 1. Packages and folders
# -----------------------------
required_packages <- c(
  "tidyverse", "lubridate", "readr", "readxl", "tsibble",
  "fable", "fabletools", "feasts", "fable.prophet",
  "mgcv", "xgboost", "lightgbm", "lpSolve", "patchwork",
  "forcats", "scales"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install missing packages before running this script: ",
    paste(missing_packages, collapse = ", "),
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
if (!dir.exists(PIC_DIR)) dir.create(PIC_DIR, showWarnings = FALSE, recursive = TRUE)

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

smape <- function(forecast, actual) {
  denominator <- (abs(forecast) + abs(actual)) / 2
  valid <- denominator > 0
  if (!any(valid)) return(NA_real_)
  mean(abs(forecast[valid] - actual[valid]) / denominator[valid], na.rm = TRUE) * 100
}

mean_error <- function(forecast, actual) {
  if (ME_DIRECTION == "forecast_minus_actual") {
    mean(forecast - actual, na.rm = TRUE)
  } else if (ME_DIRECTION == "actual_minus_forecast") {
    mean(actual - forecast, na.rm = TRUE)
  } else {
    stop("Unknown ME_DIRECTION: ", ME_DIRECTION, call. = FALSE)
  }
}

safe_nonnegative <- function(x) pmax(0, as.numeric(x))

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

add_lags <- function(df) {
  df %>%
    arrange(time) %>%
    mutate(
      lag1 = lag(visit, 1),
      lag2 = lag(visit, 2),
      lag4 = lag(visit, 4),
      lag8 = lag(visit, 8),
      lag96 = lag(visit, 96),
      lag672 = lag(visit, 672)
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
  x <- as.character(x)
  dplyr::recode(
    x,
    "snaivef" = "SNaive",
    "snaive" = "SNaive",
    "SNAIVE" = "SNaive",
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
    .default = x
  )
}

normalise_forecast_tbl <- function(df, default_model = NULL) {
  nm <- names(df)
  pred_col <- c(
    "forecast_patients_arrival", "prediction", "gam_predict", "lgb_predict",
    "xgb_predict", ".mean", "TimeGPT", "LSTM", "forecast"
  )
  pred_col <- pred_col[pred_col %in% nm][1]
  if (is.na(pred_col) || length(pred_col) == 0) {
    stop("Cannot find prediction column in external forecast RDS. Columns are: ",
         paste(nm, collapse = ", "), call. = FALSE)
  }
  day_col <- c("day", "days", "origin", ".id")
  day_col <- day_col[day_col %in% nm][1]
  model_col <- c("model", ".model")
  model_col <- model_col[model_col %in% nm][1]
  actual_col <- c("actualnumber", "actual", "visit", "y")
  actual_col <- actual_col[actual_col %in% nm][1]

  out <- df %>%
    as_tibble() %>%
    mutate(
      time = as.POSIXct(time, tz = "UTC"),
      forecast_patients_arrival = safe_nonnegative(.data[[pred_col]])
    )

  if (!is.na(day_col) && length(day_col) > 0) {
    out <- out %>% mutate(day = as.integer(.data[[day_col]]))
  } else {
    out <- out %>% mutate(day = as.integer(floor(as.numeric(difftime(time, EVAL_START, units = "days"))) + 1))
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

  out %>% select(time, day, model, actualnumber, forecast_patients_arrival)
}

# -----------------------------
# 3. Data preparation
# -----------------------------
prepare_subdaily_data <- function() {
  if (!file.exists(ARRIVAL_FILE)) stop("Arrival file not found: ", ARRIVAL_FILE, call. = FALSE)

  msg("Loading and aggregating ED arrivals: ", ARRIVAL_FILE)
  ae_original <- readr::read_csv(
    ARRIVAL_FILE,
    col_types = cols(arrival_time = col_datetime(format = "%d/%m/%Y %H:%M"))
  )

  if (!"arrival_time" %in% names(ae_original)) {
    stop("hospital2.csv must contain arrival_time.", call. = FALSE)
  }

  # Correct total ED arrivals per 15-minute interval. Do not key by patient id.
  ae_subdaily <- ae_original %>%
    filter(!is.na(arrival_time)) %>%
    mutate(time = floor_date(arrival_time, "15 minutes"), time = force_tz(time, "UTC")) %>%
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
      holiday_wales <- tibble(date = unique(ae_subdaily$date))
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
      across(c(is_public_holiday, is_school_holiday, xmas, new_year), ~ replace_na(as.integer(.x), 0L)),
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
          is_public_holiday + is_school_holiday + xmas + new_year +
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
    visit ~ s(slot, bs = "cc", k = 20) + s(wday_i, bs = "re") +
      s(trend, k = 20) + is_public_holiday_f + is_school_holiday_f + xmas_f + new_year_f,
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
  if (!is.null(feature_names)) X <- align_matrix(X, feature_names)
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
  fit <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
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
  fit <- lgb.train(params = params, data = dtrain, nrounds = 300, verbose = -1)
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
    warning("RUN_TIMEGPT_API is FALSE and TimeGPT RDS was not found. TimeGPT will be skipped: ", TIMEGPT_RDS)
    return(tibble(time = as.POSIXct(character()), day = integer(), model = character(),
                  actualnumber = numeric(), forecast_patients_arrival = numeric()))
  }

  if (!requireNamespace("nixtlar", quietly = TRUE)) {
    stop("RUN_TIMEGPT_API = TRUE but package 'nixtlar' is not installed.", call. = FALSE)
  }
  if (Sys.getenv("NIXTLA_API_KEY") == "") {
    stop("RUN_TIMEGPT_API = TRUE but NIXTLA_API_KEY is not set.", call. = FALSE)
  }

  # This block intentionally avoids hard-coded API keys. Depending on your local
  # nixtlar version, the function name may need adjustment.
  if ("nixtla_set_api_key" %in% getNamespaceExports("nixtlar")) {
    nixtlar::nixtla_set_api_key(Sys.getenv("NIXTLA_API_KEY"))
  }

  out <- vector("list", nrow(eval_days))
  for (j in seq_len(nrow(eval_days))) {
    day <- eval_days$day[j]
    day_start <- eval_days$day_start[j]
    test_data <- ae_subdaily %>% filter(time >= day_start, time < day_start + days(1))
    train_data <- ae_subdaily %>% filter(time < day_start)

    hist <- train_data %>% transmute(unique_id = "ED", ds = time, y = visit)
    future_x <- test_data %>% transmute(unique_id = "ED", ds = time)

    fc <- nixtlar::nixtla_client_forecast(hist, h = FORECAST_HORIZON, X_df = future_x)
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
    return(tibble(time = as.POSIXct(character()), day = integer(), model = character(),
                  actualnumber = numeric(), forecast_patients_arrival = numeric()))
  }
  msg("Reading LSTM forecasts from RDS: ", LSTM_RDS)
  normalise_forecast_tbl(readRDS(LSTM_RDS), default_model = "LSTM")
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
    if (day %% 10 == 1 || day == nrow(eval_days)) msg("Forecasting day ", day, " / ", nrow(eval_days))

    train_data <- ae_subdaily %>% filter(time < day_start)
    test_data <- ae_subdaily %>% filter(time >= day_start, time < day_start + days(1))

    if (nrow(test_data) != SLOTS_PER_DAY) {
      warning("Skipping day ", day, ": expected 96 test slots, got ", nrow(test_data))
      next
    }

    parts <- list()

    parts$fable <- tryCatch(forecast_fable_models_one_day(train_data, test_data),
                            error = function(e) { warning("Fable models failed on day ", day, ": ", e$message); NULL })
    parts$prophet <- tryCatch(forecast_prophet_one_day(train_data, test_data),
                              error = function(e) { warning("Prophet failed on day ", day, ": ", e$message); NULL })
    parts$mlr <- tryCatch(forecast_mlr_one_day(train_data, test_data),
                          error = function(e) { warning("MLR failed on day ", day, ": ", e$message); NULL })
    parts$gam <- tryCatch(forecast_gam_one_day(train_data, test_data),
                          error = function(e) { warning("GAM failed on day ", day, ": ", e$message); NULL })
    parts$xgb <- tryCatch(forecast_xgboost_one_day(train_data, test_data),
                          error = function(e) { warning("XGBoost failed on day ", day, ": ", e$message); NULL })
    parts$lgb <- tryCatch(forecast_lightgbm_one_day(train_data, test_data),
                          error = function(e) { warning("LightGBM failed on day ", day, ": ", e$message); NULL })

    main_out[[j]] <- bind_rows(parts) %>%
      mutate(day = day) %>%
      left_join(test_data %>% as_tibble() %>% select(time, actualnumber = visit), by = "time") %>%
      select(time, day, model, actualnumber, forecast_patients_arrival)
  }

  point_forecasts <- bind_rows(main_out)

  # Add TimeGPT and LSTM. LSTM remains RDS-based by request.
  timegpt_fc <- forecast_timegpt_or_read(ae_subdaily, eval_days)
  lstm_fc <- read_lstm_forecasts()

  # Fill actuals for external forecasts from the prepared test data if missing.
  actual_lookup <- ae_subdaily %>%
    as_tibble() %>%
    filter(time >= min(eval_days$day_start), time <= max(eval_days$day_end)) %>%
    select(time, actual_lookup = visit)

  external_fc <- bind_rows(timegpt_fc, lstm_fc) %>%
    mutate(model = normalise_model_name(model)) %>%
    left_join(actual_lookup, by = "time") %>%
    mutate(actualnumber = coalesce(actualnumber, actual_lookup)) %>%
    select(time, day, model, actualnumber, forecast_patients_arrival)

  point_forecasts <- bind_rows(point_forecasts, external_fc) %>%
    filter(day >= 1, day <= TARGET_DAYS) %>%
    mutate(
      model = normalise_model_name(model),
      forecast_patients_arrival = safe_nonnegative(forecast_patients_arrival),
      actualnumber = as.numeric(actualnumber)
    ) %>%
    arrange(model, day, time) %>%
    distinct(model, day, time, .keep_all = TRUE)

  saveRDS(point_forecasts, POINT_FORECAST_CACHE)
  readr::write_csv(point_forecasts, file.path(CACHE_DIR, "point_forecast_results_all_models.csv"))
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
      RMSE = sqrt(mean((forecast_patients_arrival - actualnumber)^2, na.rm = TRUE)),
      MAE = mean(abs(forecast_patients_arrival - actualnumber), na.rm = TRUE),
      sMAPE = smape(forecast_patients_arrival, actualnumber),
      MASE = {
        denom <- mean(abs(diff(actualnumber)), na.rm = TRUE)
        ifelse(is.na(denom) || denom == 0, NA_real_, mean(abs(forecast_patients_arrival - actualnumber), na.rm = TRUE) / denom)
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
    select(time, day, model, actualnumber, forecast_patients_arrival,
           cumulative_staff_actual, cumulative_staff_forecast)
}

# -----------------------------
# 6. Deterministic set-covering staffing optimisation
# -----------------------------
load_shift_matrix <- function() {
  if (!file.exists(SHIFT_FILE)) stop("Shift schedule file not found: ", SHIFT_FILE, call. = FALSE)
  shiftpattern <- readxl::read_xlsx(SHIFT_FILE)
  shift_cols <- grep("^Shift", names(shiftpattern), value = TRUE)
  if (length(shift_cols) == 0) stop("No columns starting with 'Shift' found in ", SHIFT_FILE, call. = FALSE)

  f.con <- shiftpattern %>% select(all_of(shift_cols)) %>% as.matrix()
  storage.mode(f.con) <- "numeric"

  # Expected shape is 96 rows x 24 shift types. Transpose if legacy file layout is 24 x 96.
  if (nrow(f.con) != SLOTS_PER_DAY && ncol(f.con) == SLOTS_PER_DAY) {
    f.con <- t(f.con)
  }
  if (nrow(f.con) != SLOTS_PER_DAY) {
    stop("Shift coverage matrix must have 96 rows after orientation check. Current dimensions: ",
         paste(dim(f.con), collapse = " x "), call. = FALSE)
  }
  colnames(f.con) <- paste0("shift", seq_len(ncol(f.con)))
  f.con
}

solve_staffing_lp <- function(req_vec, f.con) {
  req_vec <- as.numeric(req_vec)
  req_vec[is.na(req_vec)] <- 0
  req_vec <- ceiling(pmax(0, req_vec))
  if (length(req_vec) != nrow(f.con)) {
    stop("Requirement vector length must equal number of slots. Got ", length(req_vec),
         " expected ", nrow(f.con), call. = FALSE)
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

  # Forecast-based optimisation for every model-day.
  forecast_groups <- pointforecast_staffdemand %>%
    arrange(model, day, time) %>%
    group_by(model, day) %>%
    summarise(
      forecast_req = list(cumulative_staff_forecast),
      actual_req = list(cumulative_staff_actual),
      .groups = "drop"
    )

  forecast_solutions <- vector("list", nrow(forecast_groups))
  for (i in seq_len(nrow(forecast_groups))) {
    if (i %% 250 == 1 || i == nrow(forecast_groups)) msg("LP forecast solution ", i, " / ", nrow(forecast_groups))
    x <- solve_staffing_lp(forecast_groups$forecast_req[[i]], f.con)
    forecast_solutions[[i]] <- tibble(
      model = forecast_groups$model[i],
      day = forecast_groups$day[i],
      shift = shift_names,
      staffforecastoptimize = x
    )
  }
  forecast_long <- bind_rows(forecast_solutions)

  # Perfect-information optimisation only depends on the actual requirement by day.
  perfect_groups <- pointforecast_staffdemand %>%
    arrange(day, time) %>%
    group_by(day, time) %>%
    summarise(actual_req = first(cumulative_staff_actual), .groups = "drop") %>%
    group_by(day) %>%
    summarise(actual_req = list(actual_req), .groups = "drop")

  perfect_solutions <- vector("list", nrow(perfect_groups))
  for (i in seq_len(nrow(perfect_groups))) {
    if (i %% 50 == 1 || i == nrow(perfect_groups)) msg("LP perfect-information solution ", i, " / ", nrow(perfect_groups))
    x <- solve_staffing_lp(perfect_groups$actual_req[[i]], f.con)
    perfect_solutions[[i]] <- tibble(
      day = perfect_groups$day[i],
      shift = shift_names,
      Perfect_information = x
    )
  }
  perfect_long <- bind_rows(perfect_solutions)

  staff_demand_all_information <- forecast_long %>%
    left_join(perfect_long, by = c("day", "shift")) %>%
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
    select(model, day, date, sum_staffnum_forecast_daily, sum_staffnum_perfect_daily,
           forecast_staff_hours, perfect_staff_hours, differencestaffnum,
           abs_differencestaffnum, staffing_hours_difference, abs_staffing_hours_difference) %>%
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
      ME = cor(differencestaffnum, ME, use = "complete.obs"),
      RMSE = cor(differencestaffnum, RMSE, use = "complete.obs"),
      MAE = cor(differencestaffnum, MAE, use = "complete.obs"),
      MASE = cor(differencestaffnum, MASE, use = "complete.obs"),
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

  accuracy_summary <- accuracy_summary %>% mutate(model = factor(model, levels = model_order))
  operational_summary <- operational_summary %>% mutate(model = factor(model, levels = model_order))

  # Fig: Average absolute daily staffing imbalance.
  p_avg <- operational_summary %>%
    ggplot(aes(y = fct_reorder(model, Avg_Abs_Daily_Staffing_Imbalance),
               x = Avg_Abs_Daily_Staffing_Imbalance)) +
    geom_segment(aes(x = 0, xend = Avg_Abs_Daily_Staffing_Imbalance, yend = model),
                 linetype = "dotted", color = "grey50") +
    geom_point(size = 3) +
    labs(x = "Average Absolute Daily Staffing Imbalance", y = "Forecasting Models") +
    theme_bw() + theme(panel.grid.minor = element_blank())

  path <- file.path(PLOT_DIR, "Avg_staffing_plot.png")
  ggsave(path, p_avg, width = 6.5, height = 4.5, units = "in", dpi = 600)
  copy_to_pic(path)

  # Fig: total forecasted staffing and total staffing imbalance.
  perfect_total <- unique(operational_summary$Total_Perfect_Staffing)
  perfect_ref <- if (length(perfect_total) > 0) perfect_total[1] else NA_real_

  sum_by_model_prepped <- operational_summary %>%
    transmute(
      model,
      `Total Forecasted Staffing` = Total_Forecasted_Staffing,
      `Staffing Imbalance` = Total_Daily_Staffing_Imbalance
    ) %>%
    pivot_longer(-model, names_to = "Types", values_to = "Counts") %>%
    mutate(
      Reference = if_else(Types == "Total Forecasted Staffing", perfect_ref, 0),
      Difference = abs(Counts - Reference)
    )

  p_total <- sum_by_model_prepped %>%
    group_by(Types) %>%
    mutate(model_ordered = fct_reorder(model, Difference, .desc = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(y = model_ordered, x = Counts)) +
    geom_segment(aes(x = 0, xend = Counts, yend = model_ordered),
                 color = "grey50", linetype = "dotted") +
    geom_point(size = 3) +
    geom_vline(aes(xintercept = Reference), color = "red", linetype = "dashed") +
    facet_grid(Types ~ ., scales = "free_x") +
    labs(x = "Staffing Counts", y = "Forecasting Models") +
    theme_bw() + theme(panel.grid.minor = element_blank())

  path <- file.path(PLOT_DIR, "forecast_evaplot.png")
  ggsave(path, p_total, width = 7.2, height = 5.2, units = "in", dpi = 600)
  copy_to_pic(path)

  # Fig: overstaffing and understaffing days.
  over_under <- operational_summary %>%
    select(model, Overstaffing_Days, Understaffing_Days) %>%
    pivot_longer(c(Overstaffing_Days, Understaffing_Days), names_to = "Types", values_to = "Counts") %>%
    mutate(Types = recode(Types, Overstaffing_Days = "Overstaffing Days", Understaffing_Days = "Understaffing Days"))

  p_over_under <- over_under %>%
    group_by(Types) %>%
    mutate(model_ordered = fct_reorder(model, Counts, .desc = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(y = model_ordered, x = Counts)) +
    geom_segment(aes(x = 0, xend = Counts, yend = model_ordered),
                 color = "grey50", linetype = "dotted") +
    geom_point(size = 3) +
    facet_grid(Types ~ ., scales = "free_x") +
    labs(x = "Number of Days", y = "Forecasting Models") +
    theme_bw() + theme(panel.grid.minor = element_blank())

  path <- file.path(PLOT_DIR, "forecast_evaplot_over_understaffing.png")
  ggsave(path, p_over_under, width = 7.2, height = 5.2, units = "in", dpi = 600)
  copy_to_pic(path)

  # Fig: daily forecast metrics vs daily staffing imbalance.
  metric_long <- accuracy_summary %>%
    select(model, day, differencestaffnum, ME, RMSE, MAE, sMAPE, MASE) %>%
    pivot_longer(c(ME, RMSE, MAE, sMAPE, MASE), names_to = "Metric", values_to = "Value")

  p_scatter <- metric_long %>%
    ggplot(aes(x = Value, y = differencestaffnum, shape = model)) +
    geom_point(size = 1.5, alpha = 0.45) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    facet_wrap(~ Metric, scales = "free_x", ncol = 2) +
    labs(x = "Daily forecast error metric", y = "Daily Staffing Imbalance Count") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")

  path <- file.path(PLOT_DIR, "newmodelaccuracy_summary.png")
  ggsave(path, p_scatter, width = 12, height = 8, units = "in", dpi = 300)
  copy_to_pic(path)

  # Example model plots expected by manuscript.
  for (m in c("ETS", "Prophet")) {
    p_m <- accuracy_summary %>%
      filter(as.character(model) == m) %>%
      select(day, differencestaffnum, ME, RMSE, MAE, MASE) %>%
      pivot_longer(c(ME, RMSE, MAE, MASE), names_to = "Metric", values_to = "Value") %>%
      ggplot(aes(x = Value, y = differencestaffnum)) +
      geom_point(size = 1.8, alpha = 0.55) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      facet_wrap(~ Metric, scales = "free_x") +
      labs(x = "Daily forecast error metric", y = "Daily Staffing Imbalance Count") +
      theme_bw() + theme(panel.grid.minor = element_blank())

    path <- file.path(PLOT_DIR, paste0("newmodelaccuracy_", tolower(m), ".png"))
    ggsave(path, p_m, width = 8, height = 5.5, units = "in", dpi = 300)
    copy_to_pic(path)
  }

  # Correlation heatmap.
  corr_long <- correlation_results %>%
    pivot_longer(-model, names_to = "Metric", values_to = "Correlation")

  p_heat <- corr_long %>%
    ggplot(aes(x = Metric, y = fct_reorder(model, abs(Correlation), .fun = max), fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", Correlation)), size = 3) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-1, 1)) +
    labs(x = "Forecast error metric", y = "Forecasting Model", fill = "Correlation") +
    theme_bw() + theme(panel.grid = element_blank())

  path <- file.path(PLOT_DIR, "correlation_heatmap.png")
  ggsave(path, p_heat, width = 7.5, height = 5.0, units = "in", dpi = 600)
  copy_to_pic(path)

  invisible(TRUE)
}

# -----------------------------
# 9. Main execution
# -----------------------------
msg("=== Starting end-to-end reproduction pipeline ===")

ae_subdaily <- prepare_subdaily_data()
eval_days <- make_eval_days(ae_subdaily)

if (nrow(eval_days) < TARGET_DAYS) {
  warning("Only ", nrow(eval_days), " complete evaluation days are available from ", as.character(EVAL_START), ".")
}

# Save the rolling-origin train/test structures used by older scripts.
ae_subdaily_tsb <- ae_subdaily %>% as_tsibble(index = time)
initial_n <- nrow(ae_subdaily_tsb %>% filter(time < EVAL_START))
ae_subdaily_train_tscv <- ae_subdaily_tsb %>%
  slice(1:(n() - FORECAST_HORIZON)) %>%
  stretch_tsibble(.init = initial_n, .step = SLOTS_PER_DAY) %>%
  filter(.id <= nrow(eval_days)) %>%
  rename(origin = .id)

ae_subdaily_test_tscv <- map_dfr(seq_len(nrow(eval_days)), function(i) {
  ae_subdaily_tsb %>%
    filter(time >= eval_days$day_start[i], time < eval_days$day_start[i] + days(1)) %>%
    as_tibble() %>%
    mutate(origin = eval_days$day[i])
}) %>%
  as_tsibble(key = origin, index = time)

saveRDS(ae_subdaily_train_tscv, file.path(CACHE_DIR, "ae_subdaily_train_tscv.rds"))
saveRDS(ae_subdaily_test_tscv, file.path(CACHE_DIR, "ae_subdaily_test_tscv.rds"))

point_forecast_results <- run_forecast_pipeline(ae_subdaily, eval_days)
saveRDS(point_forecast_results, file.path(CACHE_DIR, "point_forecast_results.rds"))
write_csv(point_forecast_results, file.path(CACHE_DIR, "point_forecast_results.csv"))

msg("Models included: ", paste(sort(unique(point_forecast_results$model)), collapse = ", "))

pointforecast_staffdemand <- convert_to_staff_demand(point_forecast_results)
saveRDS(pointforecast_staffdemand, file.path(CACHE_DIR, "pointforecast_staffdemand.rds"))
write_csv(pointforecast_staffdemand, file.path(CACHE_DIR, "pointforecast_staffdemand.csv"))

f.con <- load_shift_matrix()
opt_results <- run_staffing_optimisation(pointforecast_staffdemand, f.con)

tables <- make_result_tables(
  point_forecast_results = point_forecast_results,
  pointforecast_staffdemand = pointforecast_staffdemand,
  summary_day = opt_results$summary_day
)

plot_manuscript_figures(tables)

msg("=== Done ===")
msg("Key outputs:")
msg("  ", file.path(CACHE_DIR, "model_accuracy_summary.csv"))
msg("  ", file.path(CACHE_DIR, "model_operational_summary.csv"))
msg("  ", file.path(CACHE_DIR, "overall_accuracy_summary.csv"))
msg("  ", file.path(CACHE_DIR, "correlation_results.csv"))
msg("  ", PLOT_DIR, "/ and ", PIC_DIR, "/ figures")
