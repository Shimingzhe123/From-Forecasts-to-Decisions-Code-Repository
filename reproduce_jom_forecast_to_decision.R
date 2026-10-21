# ============================================================================
# Reproducible R workflow for the JOM manuscript
# Title: From Forecast to Decision: The Value of Uncertainty in Healthcare Staffing Operations

# ============================================================================
# 0. USER SETTINGS
# ============================================================================

RUN_FORECASTS          <- TRUE
RUN_MAIN_OPTIMIZATION  <- TRUE
RUN_SENSITIVITY        <- TRUE
MAKE_PLOTS             <- TRUE

# To debug quickly, set TARGET_DAYS to a small number and/or LIMIT_MODELS.
TARGET_DAYS <- 365
TRAIN_DAYS  <- 28
BOOT_TIMES  <- 500
K_COMP      <- 20

# Optional: limit model set for testing, e.g. c("ARIMA", "LGBM").
# Keep NULL for the full six-model manuscript run.
LIMIT_MODELS <- NULL

# Optional: set to TRUE to export slot-level daily curves. This can be large.
SAVE_DAILY_CURVES <- FALSE

# Sensitivity setup used in the manuscript table.
SENSITIVITY_DAY_COUNT <- 30
SENSITIVITY_GRID <- expand.grid(
  c_short_hourly = c(20, 40, 80),
  c_over_hourly  = c(0, 0.2, 1),
  ot_cap         = c(1, 2, NA)
)

# Solver controls.
GLPK_VERBOSE        <- FALSE
GLPK_TIME_LIMIT_SEC <- 20

# Reproducibility.
set.seed(1)

# Directories.
DATA_DIR      <- "01data"
OUTPUT_DIR    <- "outputs"
SENS_DIR      <- "outputs_sens"
PLOT_DIR      <- "plots_jom"
CURVE_DIR     <- file.path(OUTPUT_DIR, "daily_curves")

# Input files.
ARRIVAL_FILE  <- file.path(DATA_DIR, "hospital2.csv")
SHIFT_FILE    <- file.path(DATA_DIR, "shiftschedule.xlsx")
FORECAST_RDS  <- file.path(DATA_DIR, "forecast_data.rds")
MAIN_CSV      <- file.path(DATA_DIR, "optimization_results_main.csv")
DIAG_CSV      <- file.path(OUTPUT_DIR, "diag_summary.csv")
SUMMARY_CSV   <- file.path(OUTPUT_DIR, "model_summary.csv")

# Slot and StaffReq parameters.
SLOTS_PER_DAY        <- 96
SLOT_MINUTES         <- 15
SLOTS_PER_HOUR       <- 60 / SLOT_MINUTES
SLOT_HOURS           <- SLOT_MINUTES / 60
WINDOW_SIZE_STAFFREQ <- 14
PATIENT_STAFF_RATIO  <- 0.25

# Baseline cost settings in hourly units; converted to slot units below.
REG_COST_PER_HOUR     <- 1.0
C_OT_HOURLY_BASE      <- 1.5
C_SHORT_HOURLY_BASE   <- 40.0
C_OVER_HOURLY_BASE    <- 0.2
OT_CAP_BASE           <- 2

# EVPI method.
# "realized" is computationally feasible and matches the manuscript wording that
# the perfect-information benchmark can be implemented for each realized day.
# "q_compressed" solves a perfect-information model for compressed Q scenarios;
# this is more expensive and usually unnecessary for the main manuscript run.
EVPI_METHOD <- "realized"  # choices: "realized", "q_compressed"
EVPI_Q_COMP <- 10

# ============================================================================
# 1. PACKAGES AND UTILITIES
# ============================================================================

required_packages <- c(
  "dplyr", "tidyr", "readr", "readxl", "purrr", "tsibble",
  "fable", "fabletools", "feasts", "lubridate", "lightgbm", "mgcv",
  "prophet", "scoringRules", "ompr", "ompr.roi", "ROI",
  "ROI.plugin.glpk", "cluster", "ggplot2", "ggrepel", "grid"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(purrr)
  library(tsibble)
  library(fable)
  library(fabletools)
  library(feasts)
  library(lubridate)
  library(lightgbm)
  library(mgcv)
  library(prophet)
  library(scoringRules)
  library(ompr)
  library(ompr.roi)
  library(ROI)
  library(ROI.plugin.glpk)
  library(cluster)
  library(ggplot2)
  library(ggrepel)
  library(grid)
})

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(SENS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)
if (SAVE_DAILY_CURVES) dir.create(CURVE_DIR, showWarnings = FALSE, recursive = TRUE)

msg <- function(...) cat(paste0(..., "\n"))

safe_mean <- function(x) mean(x, na.rm = TRUE)
safe_median <- function(x) median(x, na.rm = TRUE)

# ============================================================================
# 2. DATA AND STAFF-REQUIREMENT HELPERS
# ============================================================================

rolling_sum <- function(x, window_size = 14) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  vapply(seq_len(n), function(i) {
    if (i < window_size) {
      sum(x[1:i])
    } else {
      sum(x[(i - window_size + 1):i])
    }
  }, numeric(1))
}

StaffReq_window <- function(arrivals, window_size = 14, ratio = 0.25) {
  cum <- rolling_sum(round(arrivals), window_size = window_size)
  ceiling(cum * ratio)
}

load_arrivals_15min <- function(arrival_file = ARRIVAL_FILE) {
  if (!file.exists(arrival_file)) {
    stop("Arrival file not found: ", arrival_file, call. = FALSE)
  }

  ae_original <- readr::read_csv(
    arrival_file,
    col_types = cols(arrival_time = col_datetime(format = "%d/%m/%Y %H:%M"))
  )

  if (!"arrival_time" %in% names(ae_original)) {
    stop("The arrival file must contain a column named 'arrival_time'.", call. = FALSE)
  }

  # Important: do not use patient id as a tsibble key for aggregation, because
  # the manuscript requires total ED arrivals per 15-minute slot.
  out <- ae_original %>%
    filter(!is.na(arrival_time)) %>%
    mutate(time = lubridate::floor_date(arrival_time, "15 minutes")) %>%
    count(time, name = "visit") %>%
    arrange(time) %>%
    tidyr::complete(
      time = seq(min(time), max(time), by = "15 min"),
      fill = list(visit = 0)
    ) %>%
    mutate(
      time = lubridate::force_tz(time, "UTC"),
      visit = as.numeric(visit),
      eval_date = as.Date(time, tz = "UTC")
    ) %>%
    arrange(time)

  out
}

select_evaluation_days <- function(ae_subdaily, target_days = TARGET_DAYS) {
  complete_days <- ae_subdaily %>%
    count(eval_date, name = "n_slots") %>%
    filter(n_slots == SLOTS_PER_DAY) %>%
    arrange(eval_date)

  if (nrow(complete_days) == 0) {
    stop("No complete 96-slot days found after 15-minute aggregation.", call. = FALSE)
  }

  if (nrow(complete_days) < target_days) {
    warning("Only ", nrow(complete_days), " complete days available; using all of them.")
  }

  tail(complete_days$eval_date, min(target_days, nrow(complete_days)))
}

load_shift_matrix <- function(shift_file = SHIFT_FILE) {
  if (!file.exists(shift_file)) {
    stop("Shift schedule file not found: ", shift_file, call. = FALSE)
  }

  shiftpattern <- readxl::read_xlsx(shift_file)
  shift_cols <- grep("^Shift", names(shiftpattern), value = TRUE)

  if (length(shift_cols) == 0) {
    stop("No shift columns found. Expected columns beginning with 'Shift'.", call. = FALSE)
  }

  f.con <- shiftpattern %>%
    select(all_of(shift_cols)) %>%
    as.matrix()

  storage.mode(f.con) <- "numeric"

  if (nrow(f.con) != SLOTS_PER_DAY) {
    stop("Coverage matrix must have 96 rows, one for each 15-minute slot.", call. = FALSE)
  }

  f.con
}

make_regular_shift_cost <- function(f.con, reg_cost_per_hour = REG_COST_PER_HOUR) {
  covered_hours <- colSums(f.con) * SLOT_HOURS
  reg_cost_per_hour * covered_hours
}

# ============================================================================
# 3. FORECAST MODELS AND SCENARIO GENERATION
# ============================================================================

daily_crps_from_draws <- function(sims_long, actual_df) {
  tmp <- sims_long %>%
    group_by(time) %>%
    summarise(draws = list(draw), .groups = "drop") %>%
    inner_join(actual_df, by = "time")

  crps_vec <- purrr::map2_dbl(
    tmp$obs, tmp$draws,
    ~ scoringRules::crps_sample(y = .x, dat = .y)
  )

  mean(crps_vec, na.rm = TRUE)
}

fit_models_fable3 <- function(train_data) {
  train_ts <- train_data %>%
    select(time, visit) %>%
    as_tsibble(index = time)

  train_ts %>%
    model(
      SNAIVE  = SNAIVE(visit ~ lag("day")),
      STL_ETS = decomposition_model(
        STL(visit ~ season(period = 96)),
        ETS(season_adjust)
      ),
      ARIMA_F = ARIMA(visit ~ fourier(period = 96, K = 5))
    )
}

predict_and_sims_fable_all <- function(models_fit, day_block, boot_times, seed_base) {
  new_data <- day_block %>% select(time, visit)

  fc_tbl <- forecast(models_fit, new_data = new_data) %>%
    as_tibble() %>%
    select(.model, time, .mean) %>%
    arrange(.model, time)

  set.seed(seed_base)
  sims_tbl <- models_fit %>%
    generate(new_data = new_data, times = boot_times, bootstrap = TRUE) %>%
    as_tibble() %>%
    transmute(.model, time, rep = .rep, draw = pmax(0, .sim)) %>%
    arrange(.model, rep, time)

  list(point = fc_tbl, sims = sims_tbl)
}

build_lgbm_frame <- function(df) {
  df %>%
    mutate(
      slot   = as.integer((hour(time) * 60 + minute(time)) / 15) + 1L,
      hour   = hour(time),
      minute = minute(time),
      wday_i = wday(time, week_start = 1),
      wday_f = factor(wday_i, levels = 1:7)
    )
}

add_lags <- function(df) {
  df %>%
    arrange(time) %>%
    mutate(
      lag1  = dplyr::lag(visit, 1),
      lag2  = dplyr::lag(visit, 2),
      lag4  = dplyr::lag(visit, 4),
      lag8  = dplyr::lag(visit, 8),
      lag96 = dplyr::lag(visit, 96)
    )
}

make_x_y <- function(train_df) {
  form <- ~ hour + minute + slot + wday_f + lag1 + lag2 + lag4 + lag8 + lag96
  X <- model.matrix(form, data = train_df)
  y <- train_df$visit
  list(X = X, y = y, form = form)
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

fit_lgbm_native <- function(train_data) {
  df <- train_data %>%
    select(time, visit) %>%
    build_lgbm_frame() %>%
    add_lags() %>%
    filter(!is.na(lag96))

  if (nrow(df) < 300) return(NULL)

  xy <- make_x_y(df)
  X <- xy$X
  y <- xy$y

  dtrain <- lgb.Dataset(data = X, label = y)

  params <- list(
    objective = "regression",
    metric = "l2",
    learning_rate = 0.05,
    num_leaves = 31,
    min_data_in_leaf = 20,
    feature_fraction = 0.9,
    bagging_fraction = 0.9,
    bagging_freq = 1,
    verbosity = -1
  )

  booster <- lgb.train(params = params, data = dtrain, nrounds = 300, verbose = -1)
  yhat_in <- predict(booster, X)
  resid_pool <- y - yhat_in

  list(
    booster = booster,
    feature_names = colnames(X),
    form = xy$form,
    resid_pool = resid_pool
  )
}

lgbm_forecast_point_and_sims_96 <- function(model_obj, history_df, start_time, boot_times) {
  booster <- model_obj$booster
  feature_names <- model_obj$feature_names
  form <- model_obj$form
  resid_pool <- model_obj$resid_pool

  hist <- history_df %>%
    select(time, visit) %>%
    arrange(time) %>%
    mutate(visit = as.numeric(visit))

  future_times <- seq(from = start_time, by = "15 min", length.out = SLOTS_PER_DAY)
  point <- numeric(SLOTS_PER_DAY)
  sims <- matrix(0, nrow = SLOTS_PER_DAY, ncol = boot_times)

  for (h in seq_len(SLOTS_PER_DAY)) {
    t_now <- future_times[h]

    tmp <- bind_rows(hist, tibble(time = t_now, visit = NA_real_)) %>%
      build_lgbm_frame() %>%
      add_lags()

    row <- tmp %>% filter(time == t_now)

    if (any(is.na(row %>% select(lag1, lag2, lag4, lag8, lag96)))) {
      stop("LGBM lag error at horizon step ", h, ".")
    }

    X1 <- model.matrix(form, data = row)
    X1 <- align_matrix(X1, feature_names)

    pred <- max(0, as.numeric(predict(booster, X1)))
    point[h] <- pred

    eps <- sample(resid_pool, size = boot_times, replace = TRUE)
    sims[h, ] <- pmax(0, pred + eps)

    # Recursive point path, consistent with the manuscript workflow.
    hist <- bind_rows(hist, tibble(time = t_now, visit = pred))
  }

  list(
    point = tibble(.model = "LGBM", time = future_times, .mean = point),
    sims  = tibble(
      .model = "LGBM",
      time   = rep(future_times, times = boot_times),
      rep    = rep(seq_len(boot_times), each = SLOTS_PER_DAY),
      draw   = as.numeric(sims)
    )
  )
}

build_gam_frame <- function(df) {
  df %>%
    mutate(
      slot   = as.integer((hour(time) * 60 + minute(time)) / 15) + 1L,
      tod    = (slot - 1) / SLOTS_PER_DAY,
      wday_i = wday(time, week_start = 1),
      wday_f = factor(wday_i, levels = 1:7)
    )
}

fit_gam_native <- function(train_data) {
  df <- train_data %>%
    select(time, visit) %>%
    build_gam_frame() %>%
    add_lags() %>%
    filter(!is.na(lag96)) %>%
    mutate(visit = as.numeric(visit))

  if (nrow(df) < 400) return(NULL)

  gam_fit <- mgcv::bam(
    visit ~ s(tod, bs = "cc", k = 20) + s(wday_f, bs = "re") + lag1 + lag2 + lag4 + lag8 + lag96,
    data = df,
    family = gaussian(),
    knots = list(tod = c(0, 1))
  )

  list(fit = gam_fit, resid_pool = residuals(gam_fit))
}

gam_forecast_point_and_sims_96 <- function(model_obj, history_df, start_time, boot_times) {
  fit <- model_obj$fit
  resid_pool <- model_obj$resid_pool

  hist <- history_df %>%
    select(time, visit) %>%
    arrange(time) %>%
    mutate(visit = as.numeric(visit))

  future_times <- seq(from = start_time, by = "15 min", length.out = SLOTS_PER_DAY)
  point <- numeric(SLOTS_PER_DAY)
  sims <- matrix(0, nrow = SLOTS_PER_DAY, ncol = boot_times)

  for (h in seq_len(SLOTS_PER_DAY)) {
    t_now <- future_times[h]

    tmp <- bind_rows(hist, tibble(time = t_now, visit = NA_real_)) %>%
      build_gam_frame() %>%
      add_lags()

    row <- tmp %>% filter(time == t_now)

    if (any(is.na(row %>% select(lag1, lag2, lag4, lag8, lag96)))) {
      stop("GAM lag error at horizon step ", h, ".")
    }

    pred <- max(0, as.numeric(predict(fit, newdata = row)))
    point[h] <- pred

    eps <- sample(resid_pool, size = boot_times, replace = TRUE)
    sims[h, ] <- pmax(0, pred + eps)

    # Recursive point path.
    hist <- bind_rows(hist, tibble(time = t_now, visit = pred))
  }

  list(
    point = tibble(.model = "GAM", time = future_times, .mean = point),
    sims  = tibble(
      .model = "GAM",
      time   = rep(future_times, times = boot_times),
      rep    = rep(seq_len(boot_times), each = SLOTS_PER_DAY),
      draw   = as.numeric(sims)
    )
  )
}

fit_prophet_native <- function(train_data) {
  df <- train_data %>%
    select(time, visit) %>%
    transmute(ds = as.POSIXct(time, tz = "UTC"), y = as.numeric(visit))

  if (nrow(df) < SLOTS_PER_DAY * 7) return(NULL)

  m <- prophet::prophet(
    df,
    daily.seasonality = TRUE,
    weekly.seasonality = TRUE,
    yearly.seasonality = FALSE,
    seasonality.mode = "additive"
  )

  pred_in <- stats::predict(m, df)
  resid_pool <- df$y - pred_in$yhat

  list(model = m, resid_pool = resid_pool)
}

prophet_forecast_point_and_sims_96 <- function(model_obj, start_time, boot_times) {
  m <- model_obj$model
  resid_pool <- model_obj$resid_pool

  future <- data.frame(
    ds = seq(from = as.POSIXct(start_time, tz = "UTC"), by = "15 min", length.out = SLOTS_PER_DAY)
  )

  pred <- stats::predict(m, future)
  point <- pmax(0, pred$yhat)

  sims_mat <- sapply(
    seq_len(boot_times),
    function(j) pmax(0, point + sample(resid_pool, size = SLOTS_PER_DAY, replace = TRUE))
  )

  list(
    point = tibble(.model = "PROPHET", time = future$ds, .mean = point),
    sims  = tibble(
      .model = "PROPHET",
      time   = rep(future$ds, times = boot_times),
      rep    = rep(seq_len(boot_times), each = SLOTS_PER_DAY),
      draw   = as.numeric(sims_mat)
    )
  )
}

# ============================================================================
# 4. FORECAST LOOP
# ============================================================================

generate_forecast_data <- function(
    ae_subdaily,
    target_days = TARGET_DAYS,
    train_days = TRAIN_DAYS,
    boot_times = BOOT_TIMES,
    limit_models = LIMIT_MODELS,
    output_rds = FORECAST_RDS) {

  eval_dates <- select_evaluation_days(ae_subdaily, target_days = target_days)
  horizon_days <- length(eval_dates)

  model_map <- c(
    "SNAIVE"  = "SNAIVE",
    "STL_ETS" = "ETS",
    "ARIMA_F" = "ARIMA",
    "LGBM"    = "LGBM",
    "GAM"     = "GAM",
    "PROPHET" = "Prophet"
  )

  if (!is.null(limit_models)) {
    keep_raw <- names(model_map)[model_map %in% limit_models | names(model_map) %in% limit_models]
    model_map <- model_map[keep_raw]
  }

  if (length(model_map) == 0) stop("No models selected.", call. = FALSE)

  all_days_data <- vector("list", horizon_days)
  msg("Starting forecast generation for ", horizon_days, " evaluation days.")

  for (d in seq_len(horizon_days)) {
    eval_date <- eval_dates[d]
    day_block <- ae_subdaily %>%
      filter(eval_date == !!eval_date) %>%
      arrange(time)

    if (nrow(day_block) != SLOTS_PER_DAY) {
      stop("Evaluation day ", eval_date, " does not contain 96 slots.", call. = FALSE)
    }

    planning_start <- min(day_block$time)
    arrivals_actual <- as.numeric(day_block$visit)
    staff_actual_vec <- StaffReq_window(arrivals_actual, WINDOW_SIZE_STAFFREQ, PATIENT_STAFF_RATIO)

    train_start <- planning_start - lubridate::days(train_days)
    train_data <- ae_subdaily %>%
      filter(time >= train_start, time < planning_start) %>%
      arrange(time)

    if (nrow(train_data) < train_days * SLOTS_PER_DAY) {
      warning("Training window shorter than expected for day ", d, ".")
    }

    msg("Forecast day ", d, "/", horizon_days, " | date=", as.character(eval_date))

    day_results <- list()
    day_results$info <- list(
      d = d,
      date = planning_start,
      eval_date = eval_date,
      actual_req = staff_actual_vec,
      actual_arr = arrivals_actual
    )
    day_results$models <- list()

    # Fable models are fitted once together.
    fable_raw <- intersect(names(model_map), c("SNAIVE", "STL_ETS", "ARIMA_F"))
    if (length(fable_raw) > 0) {
      msg("  Fitting fable models.")
      models_fit <- fit_models_fable3(train_data)
      out_fable_P <- predict_and_sims_fable_all(models_fit, day_block, boot_times, seed_base = 1000 + d)
      out_fable_Q <- predict_and_sims_fable_all(models_fit, day_block, boot_times, seed_base = 3000 + d)
    } else {
      out_fable_P <- NULL
      out_fable_Q <- NULL
    }

    # Native ML/semi-parametric models.
    lgbm_obj <- NULL
    gam_obj <- NULL
    prophet_obj <- NULL

    if ("LGBM" %in% names(model_map)) {
      msg("  Fitting LightGBM.")
      lgbm_obj <- fit_lgbm_native(train_data)
    }
    if ("GAM" %in% names(model_map)) {
      msg("  Fitting GAM.")
      gam_obj <- fit_gam_native(train_data)
    }
    if ("PROPHET" %in% names(model_map)) {
      msg("  Fitting Prophet.")
      prophet_obj <- fit_prophet_native(train_data)
    }

    for (m_raw in names(model_map)) {
      m_print <- unname(model_map[[m_raw]])
      fc_point <- NULL
      sims_P <- NULL
      sims_Q <- NULL

      if (m_raw %in% c("SNAIVE", "STL_ETS", "ARIMA_F")) {
        fc_point <- out_fable_P$point %>% filter(.model == m_raw) %>% arrange(time)
        sims_P <- out_fable_P$sims %>% filter(.model == m_raw) %>% select(time, rep, draw) %>% arrange(rep, time)
        sims_Q <- out_fable_Q$sims %>% filter(.model == m_raw) %>% select(time, rep, draw) %>% arrange(rep, time)
      } else if (m_raw == "LGBM" && !is.null(lgbm_obj)) {
        set.seed(1000 + d + 11)
        P <- lgbm_forecast_point_and_sims_96(lgbm_obj, ae_subdaily %>% filter(time < planning_start), planning_start, boot_times)
        set.seed(3000 + d + 11)
        Q <- lgbm_forecast_point_and_sims_96(lgbm_obj, ae_subdaily %>% filter(time < planning_start), planning_start, boot_times)
        fc_point <- P$point %>% arrange(time)
        sims_P <- P$sims %>% select(time, rep, draw) %>% arrange(rep, time)
        sims_Q <- Q$sims %>% select(time, rep, draw) %>% arrange(rep, time)
      } else if (m_raw == "GAM" && !is.null(gam_obj)) {
        set.seed(1000 + d + 22)
        P <- gam_forecast_point_and_sims_96(gam_obj, ae_subdaily %>% filter(time < planning_start), planning_start, boot_times)
        set.seed(3000 + d + 22)
        Q <- gam_forecast_point_and_sims_96(gam_obj, ae_subdaily %>% filter(time < planning_start), planning_start, boot_times)
        fc_point <- P$point %>% arrange(time)
        sims_P <- P$sims %>% select(time, rep, draw) %>% arrange(rep, time)
        sims_Q <- Q$sims %>% select(time, rep, draw) %>% arrange(rep, time)
      } else if (m_raw == "PROPHET" && !is.null(prophet_obj)) {
        set.seed(1000 + d + 33)
        P <- prophet_forecast_point_and_sims_96(prophet_obj, planning_start, boot_times)
        set.seed(3000 + d + 33)
        Q <- prophet_forecast_point_and_sims_96(prophet_obj, planning_start, boot_times)
        fc_point <- P$point %>% arrange(time)
        sims_P <- P$sims %>% select(time, rep, draw) %>% arrange(rep, time)
        sims_Q <- Q$sims %>% select(time, rep, draw) %>% arrange(rep, time)
      }

      if (!is.null(fc_point)) {
        actual_df <- tibble(time = day_block$time, obs = arrivals_actual)
        crps_val <- daily_crps_from_draws(sims_P, actual_df)

        if (nrow(fc_point) != SLOTS_PER_DAY) stop("Point forecast length error for ", m_print, ".")
        if (nrow(sims_P) != SLOTS_PER_DAY * boot_times) stop("P scenario length error for ", m_print, ".")
        if (nrow(sims_Q) != SLOTS_PER_DAY * boot_times) stop("Q scenario length error for ", m_print, ".")

        day_results$models[[m_print]] <- list(
          point = as.numeric(fc_point$.mean),
          sims_P_mat = matrix(sims_P$draw, nrow = SLOTS_PER_DAY, byrow = FALSE),
          sims_Q_mat = matrix(sims_Q$draw, nrow = SLOTS_PER_DAY, byrow = FALSE),
          crps = crps_val
        )
      }
    }

    all_days_data[[d]] <- day_results
  }

  saveRDS(all_days_data, output_rds)
  msg("Forecast data saved to: ", output_rds)
  invisible(all_days_data)
}

# ============================================================================
# 5. OPTIMIZATION MODEL AND EVALUATION
# ============================================================================

scenarios_to_E_mat_from_matrix <- function(sims_mat, ratio = PATIENT_STAFF_RATIO, window_size = WINDOW_SIZE_STAFFREQ) {
  apply(sims_mat, 2, StaffReq_window, window_size = window_size, ratio = ratio)
}

compress_scenarios_kmedoids <- function(E_mat, k) {
  S <- ncol(E_mat)
  if (S == 0) stop("E_mat has zero scenarios.", call. = FALSE)

  if (k >= S) {
    return(list(E_comp = E_mat, p = rep(1 / S, S), medoid_idx = seq_len(S), cluster = seq_len(S)))
  }

  pam_fit <- cluster::pam(t(E_mat), k = k, metric = "manhattan", stand = FALSE)

  medoids <- pam_fit$id.med
  cluster_id <- pam_fit$clustering
  w_by_cluster <- as.numeric(table(factor(cluster_id, levels = seq_len(k)))) / S
  p_medoids <- w_by_cluster[cluster_id[medoids]]

  list(
    E_comp = E_mat[, medoids, drop = FALSE],
    p = p_medoids,
    medoid_idx = medoids,
    cluster = cluster_id
  )
}

solve_two_stage_day_from_E_balance_eq <- function(E_mat, p_s, f.con,
                                                  c_reg, c_ot, c_short, c_over,
                                                  ot_cap = NA,
                                                  verbose = FALSE,
                                                  time_limit_sec = NA) {
  n_slots <- nrow(f.con)
  n_shifts <- ncol(f.con)
  S <- ncol(E_mat)

  stopifnot(nrow(E_mat) == n_slots)
  stopifnot(length(p_s) == S)

  p_s <- as.numeric(p_s)
  p_s <- p_s / sum(p_s)

  model <- MIPModel() %>%
    add_variable(x[i], i = seq_len(n_shifts), type = "integer", lb = 0) %>%
    add_variable(o[t, s], t = seq_len(n_slots), s = seq_len(S), lb = 0) %>%
    add_variable(short[t, s], t = seq_len(n_slots), s = seq_len(S), lb = 0) %>%
    add_variable(over[t, s], t = seq_len(n_slots), s = seq_len(S), lb = 0) %>%
    add_constraint(
      sum_expr(f.con[t, i] * x[i], i = seq_len(n_shifts)) + o[t, s] + short[t, s] - over[t, s] == E_mat[t, s],
      t = seq_len(n_slots), s = seq_len(S)
    )

  if (!is.na(ot_cap)) {
    model <- model %>% add_constraint(o[t, s] <= ot_cap, t = seq_len(n_slots), s = seq_len(S))
  }

  model <- model %>%
    set_objective(
      sum_expr(c_reg[i] * x[i], i = seq_len(n_shifts)) +
        sum_expr(
          p_s[s] * (c_ot * o[t, s] + c_short * short[t, s] + c_over * over[t, s]),
          t = seq_len(n_slots), s = seq_len(S)
        ),
      "min"
    )

  control <- list()
  if (!is.na(time_limit_sec) && time_limit_sec > 0) {
    control$tm_limit <- as.integer(time_limit_sec * 1000)
  }

  result <- solve_model(model, with_ROI(solver = "glpk", verbose = verbose, control = control))

  x_sol <- get_solution(result, x[i]) %>% arrange(i)
  o_sol <- get_solution(result, o[t, s])
  sh_sol <- get_solution(result, short[t, s])
  ov_sol <- get_solution(result, over[t, s])

  ps_df <- tibble(s = seq_len(S), p_s = p_s)

  pad_slots <- function(df) {
    df %>%
      left_join(ps_df, by = "s") %>%
      group_by(t) %>%
      summarise(tmp = sum(value * p_s), .groups = "drop") %>%
      right_join(tibble(t = seq_len(n_slots)), by = "t") %>%
      mutate(tmp = replace_na(tmp, 0)) %>%
      arrange(t) %>%
      pull(tmp)
  }

  list(
    status = result$status,
    obj = objective_value(result),
    x = x_sol$value,
    exp_o_by_t = pad_slots(o_sol),
    exp_short_by_t = pad_slots(sh_sol),
    exp_over_by_t = pad_slots(ov_sol)
  )
}

eval_cost_fixed_x_vec <- function(x, req_vec, f.con, c_reg, c_ot, c_short, c_over, ot_cap = NA) {
  cov <- as.vector(f.con %*% x)
  gap <- pmax(req_vec - cov, 0)
  exc <- pmax(cov - req_vec, 0)

  o <- if (is.na(ot_cap)) gap else pmin(gap, ot_cap)
  short <- gap - o
  over <- exc

  reg_cost <- sum(c_reg * x)
  total_cost <- reg_cost + c_ot * sum(o) + c_short * sum(short) + c_over * sum(over)

  list(
    total_cost = total_cost,
    reg_cost = reg_cost,
    ot_slots = sum(o),
    sh_slots = sum(short),
    over_slots = sum(over),
    ot_hours = sum(o) * SLOT_HOURS,
    short_hours = sum(short) * SLOT_HOURS,
    over_hours = sum(over) * SLOT_HOURS,
    service_level = mean(short == 0),
    coverage = cov,
    gap_before_recourse = gap,
    overtime_vec = o,
    shortage_vec = short,
    over_vec = over
  )
}

eval_cost_fixed_x_mat <- function(x, E_mat, p_s, f.con, c_reg, c_ot, c_short, c_over, ot_cap = NA) {
  cov <- as.vector(f.con %*% x)
  S <- ncol(E_mat)
  p_s <- as.numeric(p_s)
  p_s <- p_s / sum(p_s)

  ot_s <- numeric(S)
  sh_s <- numeric(S)
  over_s <- numeric(S)
  sl_s <- numeric(S)

  for (s in seq_len(S)) {
    req <- E_mat[, s]
    gap <- pmax(req - cov, 0)
    exc <- pmax(cov - req, 0)
    o <- if (is.na(ot_cap)) gap else pmin(gap, ot_cap)
    short <- gap - o
    over <- exc

    ot_s[s] <- sum(o)
    sh_s[s] <- sum(short)
    over_s[s] <- sum(over)
    sl_s[s] <- mean(short == 0)
  }

  reg_cost <- sum(c_reg * x)
  exp_cost <- reg_cost +
    c_ot * sum(p_s * ot_s) +
    c_short * sum(p_s * sh_s) +
    c_over * sum(p_s * over_s)

  list(
    exp_cost = exp_cost,
    reg_cost = reg_cost,
    exp_ot = sum(p_s * ot_s),
    exp_sh = sum(p_s * sh_s),
    exp_over = sum(p_s * over_s),
    exp_ot_hours = sum(p_s * ot_s) * SLOT_HOURS,
    exp_short_hours = sum(p_s * sh_s) * SLOT_HOURS,
    exp_over_hours = sum(p_s * over_s) * SLOT_HOURS,
    exp_service_level = sum(p_s * sl_s)
  )
}

expected_perfect_info_cost <- function(E_mat, p_s, f.con, c_reg, c_ot, c_short, c_over,
                                       ot_cap = NA, verbose = FALSE, time_limit_sec = NA) {
  S <- ncol(E_mat)
  p_s <- as.numeric(p_s)
  p_s <- p_s / sum(p_s)
  costs <- numeric(S)

  for (s in seq_len(S)) {
    E_s <- matrix(E_mat[, s], ncol = 1)
    pi_res <- solve_two_stage_day_from_E_balance_eq(
      E_s, 1, f.con, c_reg, c_ot, c_short, c_over,
      ot_cap = ot_cap, verbose = verbose, time_limit_sec = time_limit_sec
    )
    pi_eval <- eval_cost_fixed_x_vec(pi_res$x, E_mat[, s], f.con, c_reg, c_ot, c_short, c_over, ot_cap)
    costs[s] <- pi_eval$total_cost
  }

  list(exp_cost = sum(p_s * costs), scenario_costs = costs)
}

run_optimization_from_forecasts <- function(
    forecast_data,
    f.con,
    c_short_hourly = C_SHORT_HOURLY_BASE,
    c_over_hourly = C_OVER_HOURLY_BASE,
    c_ot_hourly = C_OT_HOURLY_BASE,
    ot_cap = OT_CAP_BASE,
    k_comp = K_COMP,
    tag = "main",
    output_main_csv = MAIN_CSV,
    output_diag_csv = DIAG_CSV,
    save_daily_curves = SAVE_DAILY_CURVES,
    curve_dir = CURVE_DIR,
    day_indices = NULL,
    evpi_method = EVPI_METHOD) {

  c_reg <- make_regular_shift_cost(f.con)
  c_ot <- c_ot_hourly / SLOTS_PER_HOUR
  c_short <- c_short_hourly / SLOTS_PER_HOUR
  c_over <- c_over_hourly / SLOTS_PER_HOUR

  if (is.null(day_indices)) day_indices <- seq_along(forecast_data)

  results_main <- list()
  results_diag <- list()
  results_curves <- list()
  row_id <- 0
  diag_id <- 0
  curve_id <- 0

  msg("Starting optimization run: ", tag)
  msg("  Cost hourly: ot=", c_ot_hourly, ", short=", c_short_hourly, ", over=", c_over_hourly, ", ot_cap=", ifelse(is.na(ot_cap), "NA", ot_cap))

  for (idx in seq_along(day_indices)) {
    day_pos <- day_indices[idx]
    day_obj <- forecast_data[[day_pos]]
    d <- day_obj$info$d
    planning_date <- as.Date(day_obj$info$eval_date)
    staff_actual_vec <- as.numeric(day_obj$info$actual_req)
    arrivals_actual <- as.numeric(day_obj$info$actual_arr)

    for (m_print in names(day_obj$models)) {
      m_data <- day_obj$models[[m_print]]

      point_req <- StaffReq_window(pmax(0, m_data$point), WINDOW_SIZE_STAFFREQ, PATIENT_STAFF_RATIO)
      sims_P_req <- scenarios_to_E_mat_from_matrix(m_data$sims_P_mat)
      sims_Q_req <- scenarios_to_E_mat_from_matrix(m_data$sims_Q_mat)

      # Perfect information benchmark for the realized day.
      E_perf <- matrix(staff_actual_vec, ncol = 1)
      perf_res <- solve_two_stage_day_from_E_balance_eq(
        E_perf, 1, f.con, c_reg, c_ot, c_short, c_over,
        ot_cap = ot_cap, verbose = GLPK_VERBOSE, time_limit_sec = GLPK_TIME_LIMIT_SEC
      )
      cost_perf_real <- eval_cost_fixed_x_vec(perf_res$x, staff_actual_vec, f.con, c_reg, c_ot, c_short, c_over, ot_cap)

      # EV point-based solution.
      E_ev <- matrix(point_req, ncol = 1)
      ev_res <- solve_two_stage_day_from_E_balance_eq(
        E_ev, 1, f.con, c_reg, c_ot, c_short, c_over,
        ot_cap = ot_cap, verbose = GLPK_VERBOSE, time_limit_sec = GLPK_TIME_LIMIT_SEC
      )
      cost_ev_real <- eval_cost_fixed_x_vec(ev_res$x, staff_actual_vec, f.con, c_reg, c_ot, c_short, c_over, ot_cap)

      # RP stochastic solution using compressed P scenarios.
      compP <- compress_scenarios_kmedoids(sims_P_req, k = k_comp)
      rp_res <- solve_two_stage_day_from_E_balance_eq(
        compP$E_comp, compP$p, f.con, c_reg, c_ot, c_short, c_over,
        ot_cap = ot_cap, verbose = GLPK_VERBOSE, time_limit_sec = GLPK_TIME_LIMIT_SEC
      )
      cost_rp_real <- eval_cost_fixed_x_vec(rp_res$x, staff_actual_vec, f.con, c_reg, c_ot, c_short, c_over, ot_cap)

      # Independent Q evaluation for EEV, RP_Q, and VSS.
      p_Q <- rep(1 / ncol(sims_Q_req), ncol(sims_Q_req))
      EEV_Q <- eval_cost_fixed_x_mat(ev_res$x, sims_Q_req, p_Q, f.con, c_reg, c_ot, c_short, c_over, ot_cap)
      RP_Q <- eval_cost_fixed_x_mat(rp_res$x, sims_Q_req, p_Q, f.con, c_reg, c_ot, c_short, c_over, ot_cap)
      VSS <- EEV_Q$exp_cost - RP_Q$exp_cost

      # EVPI. Default uses realized-day perfect information, averaged across days later.
      if (identical(evpi_method, "q_compressed")) {
        compQ <- compress_scenarios_kmedoids(sims_Q_req, k = min(EVPI_Q_COMP, ncol(sims_Q_req)))
        PI_Q <- expected_perfect_info_cost(
          compQ$E_comp, compQ$p, f.con, c_reg, c_ot, c_short, c_over,
          ot_cap = ot_cap, verbose = GLPK_VERBOSE, time_limit_sec = GLPK_TIME_LIMIT_SEC
        )
        EVPI <- RP_Q$exp_cost - PI_Q$exp_cost
        PI_cost <- PI_Q$exp_cost
      } else {
        EVPI <- cost_rp_real$total_cost - cost_perf_real$total_cost
        PI_cost <- cost_perf_real$total_cost
      }

      EVPI <- max(0, EVPI)

      row_id <- row_id + 1
      results_main[[row_id]] <- tibble(
        day_index = d,
        date = planning_date,
        model = m_print,
        EV_cost = cost_ev_real$total_cost,
        RP_cost = cost_rp_real$total_cost,
        PI_cost = PI_cost,
        EEV = EEV_Q$exp_cost,
        RP_Q = RP_Q$exp_cost,
        VSS = VSS,
        EVPI = EVPI,
        cost_perf_real = cost_perf_real$total_cost,
        cost_ev_real = cost_ev_real$total_cost,
        cost_rp_real = cost_rp_real$total_cost,
        EEV_cost_Q = EEV_Q$exp_cost,
        RP_cost_Q = RP_Q$exp_cost,
        EV_short_hours = cost_ev_real$short_hours,
        RP_short_hours = cost_rp_real$short_hours,
        EV_service_level = cost_ev_real$service_level,
        RP_service_level = cost_rp_real$service_level,
        EV_overtime_hours = cost_ev_real$ot_hours,
        RP_overtime_hours = cost_rp_real$ot_hours,
        EV_over_hours = cost_ev_real$over_hours,
        RP_over_hours = cost_rp_real$over_hours,
        CRPS_day = m_data$crps,
        c_ot_hourly = c_ot_hourly,
        c_short_hourly = c_short_hourly,
        c_over_hourly = c_over_hourly,
        ot_cap = ot_cap,
        k_comp = k_comp,
        status_perf = as.character(perf_res$status),
        status_EV = as.character(ev_res$status),
        status_RP = as.character(rp_res$status)
      )

      diag_id <- diag_id + 1
      results_diag[[diag_id]] <- tibble(
        day_index = d,
        date = planning_date,
        model = m_print,
        CRPS_day = m_data$crps,
        EV_short_hours = cost_ev_real$short_hours,
        RP_short_hours = cost_rp_real$short_hours,
        EV_service_level = cost_ev_real$service_level,
        RP_service_level = cost_rp_real$service_level,
        EV_cost = cost_ev_real$total_cost,
        RP_cost = cost_rp_real$total_cost,
        VSS = VSS,
        EVPI = EVPI
      )

      if (save_daily_curves) {
        curve_id <- curve_id + 1
        results_curves[[curve_id]] <- tibble(
          day_index = d,
          date = planning_date,
          model = m_print,
          t = seq_len(SLOTS_PER_DAY),
          actual_arrivals = arrivals_actual,
          actual_req = staff_actual_vec,
          point_req = point_req,
          EV_coverage = cost_ev_real$coverage,
          EV_gap_before_recourse = cost_ev_real$gap_before_recourse,
          EV_overtime = cost_ev_real$overtime_vec,
          EV_shortage = cost_ev_real$shortage_vec,
          EV_overstaff = cost_ev_real$over_vec,
          RP_coverage = cost_rp_real$coverage,
          RP_gap_before_recourse = cost_rp_real$gap_before_recourse,
          RP_overtime = cost_rp_real$overtime_vec,
          RP_shortage = cost_rp_real$shortage_vec,
          RP_overstaff = cost_rp_real$over_vec,
          RP_exp_overtime = rp_res$exp_o_by_t,
          RP_exp_shortage = rp_res$exp_short_by_t,
          RP_exp_overstaff = rp_res$exp_over_by_t
        )
      }

      msg(sprintf(
        "  %s | day %03d | %-7s | CRPS=%.3f | EV_short=%.2f | RP_short=%.2f | VSS=%.2f | EVPI=%.2f",
        tag, d, m_print, m_data$crps, cost_ev_real$short_hours, cost_rp_real$short_hours, VSS, EVPI
      ))
    }
  }

  main_df <- bind_rows(results_main) %>% arrange(day_index, model)
  diag_df <- bind_rows(results_diag) %>% arrange(day_index, model)

  if (!is.null(output_main_csv)) readr::write_csv(main_df, output_main_csv)
  if (!is.null(output_diag_csv)) readr::write_csv(diag_df, output_diag_csv)

  if (save_daily_curves && length(results_curves) > 0) {
    curves_df <- bind_rows(results_curves)
    readr::write_csv(curves_df, file.path(curve_dir, paste0("daily_curves_", tag, ".csv")))
  }

  list(main = main_df, diag = diag_df)
}

# ============================================================================
# 6. SUMMARY TABLES AND SENSITIVITY ANALYSIS
# ============================================================================

make_model_summary <- function(main_df, output_csv = SUMMARY_CSV) {
  summary_df <- main_df %>%
    group_by(model) %>%
    summarise(
      mean_CRPS = safe_mean(CRPS_day),
      median_CRPS = safe_median(CRPS_day),
      mean_EV_cost = safe_mean(EV_cost),
      mean_RP_cost = safe_mean(RP_cost),
      mean_EV_short_hours = safe_mean(EV_short_hours),
      mean_RP_short_hours = safe_mean(RP_short_hours),
      mean_EV_service_level = safe_mean(EV_service_level),
      mean_RP_service_level = safe_mean(RP_service_level),
      mean_EEV = safe_mean(EEV),
      mean_RP_Q = safe_mean(RP_Q),
      mean_VSS = safe_mean(VSS),
      mean_EVPI = safe_mean(EVPI),
      .groups = "drop"
    ) %>%
    arrange(mean_CRPS)

  readr::write_csv(summary_df, output_csv)
  summary_df
}

run_sensitivity_analysis <- function(forecast_data, f.con, grid = SENSITIVITY_GRID,
                                     sensitivity_day_count = SENSITIVITY_DAY_COUNT) {
  n_days <- min(sensitivity_day_count, length(forecast_data))
  day_indices <- seq_len(n_days)

  all_sens <- list()

  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    tag <- paste0(
      "cs", g$c_short_hourly,
      "_co", g$c_over_hourly,
      "_ot", ifelse(is.na(g$ot_cap), "NA", g$ot_cap)
    )

    base_dir <- file.path(SENS_DIR, tag)
    dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)

    res <- run_optimization_from_forecasts(
      forecast_data = forecast_data,
      f.con = f.con,
      c_short_hourly = g$c_short_hourly,
      c_over_hourly = g$c_over_hourly,
      c_ot_hourly = C_OT_HOURLY_BASE,
      ot_cap = g$ot_cap,
      k_comp = K_COMP,
      tag = tag,
      output_main_csv = file.path(base_dir, "vss_3policy_by_day_model.csv"),
      output_diag_csv = file.path(base_dir, "diag_summary.csv"),
      save_daily_curves = FALSE,
      day_indices = day_indices,
      evpi_method = "realized"
    )

    all_sens[[i]] <- res$main %>%
      mutate(
        scenario_tag = tag,
        c_short_hourly = g$c_short_hourly,
        c_over_hourly = g$c_over_hourly,
        ot_cap = g$ot_cap
      )
  }

  sens_df <- bind_rows(all_sens)
  readr::write_csv(sens_df, file.path(SENS_DIR, "MASTER_sensitivity_all.csv"))

  sens_summary <- sens_df %>%
    group_by(c_short_hourly, c_over_hourly, ot_cap) %>%
    summarise(
      mean_VSS = safe_mean(VSS),
      min_VSS = min(VSS, na.rm = TRUE),
      max_VSS = max(VSS, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_VSS))

  readr::write_csv(sens_summary, file.path(SENS_DIR, "Table_sensitivity_summary.csv"))
  sens_summary
}

# ============================================================================
# 7. VISUALIZATION FUNCTIONS
# ============================================================================

jom_theme <- theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, color = "gray30", margin = margin(b = 10)),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.text = element_text(size = 11),
    panel.grid.major.y = element_line(color = "gray85", linetype = "dashed", linewidth = 0.4),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

policy_colors <- c(
  "EV (Point-based)" = "#999999",
  "RP (Stochastic)"  = "#0072B2"
)

value_colors <- c(
  "VSS (Optimization Value)"    = "#0072B2",
  "EVPI (Prediction Potential)" = "#D55E00"
)

make_jom_plots <- function(main_df, plot_dir = PLOT_DIR) {
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

  if ("CRPS_day" %in% names(main_df)) {
    model_order <- main_df %>%
      group_by(model) %>%
      summarise(order_metric = median(CRPS_day, na.rm = TRUE), .groups = "drop") %>%
      arrange(order_metric) %>%
      pull(model)
  } else {
    model_order <- sort(unique(main_df$model))
  }

  df_plot <- main_df %>%
    mutate(model = factor(model, levels = model_order))

  # Figure 1: CRPS.
  p1 <- ggplot(df_plot, aes(x = model, y = CRPS_day)) +
    geom_jitter(width = 0.15, alpha = 0.15, size = 1, color = "gray50") +
    geom_boxplot(alpha = 0.9, outlier.shape = NA, fill = "#F5F5F5", color = "black", width = 0.5) +
    labs(
      title = "Predictive Performance across Forecasting Models",
      subtitle = "Daily average Continuous Ranked Probability Score (CRPS). Lower is better.",
      x = "Forecasting Model",
      y = "Daily Average CRPS"
    ) +
    jom_theme
  ggsave(file.path(plot_dir, "Fig1_CRPS.pdf"), plot = p1, width = 7.5, height = 4.5)

  # Figure 2: understaffing after recourse.
  df_shortage <- df_plot %>%
    select(date, model, EV_short_hours, RP_short_hours) %>%
    pivot_longer(
      cols = c(EV_short_hours, RP_short_hours),
      names_to = "Policy",
      values_to = "Shortage"
    ) %>%
    mutate(
      Policy = recode(
        Policy,
        EV_short_hours = "EV (Point-based)",
        RP_short_hours = "RP (Stochastic)"
      ),
      Policy = factor(Policy, levels = names(policy_colors))
    )

  p2 <- ggplot(df_shortage, aes(x = model, y = Shortage, fill = Policy)) +
    geom_boxplot(alpha = 0.85, outlier.size = 0.5, outlier.alpha = 0.3, width = 0.7,
                 position = position_dodge(0.75)) +
    scale_fill_manual(values = policy_colors) +
    labs(
      title = "Understaffing Risk under EV and Stochastic RP",
      subtitle = "Realized daily shortage after flexible-capacity recourse.",
      x = "Forecasting Model",
      y = "Realized Daily Understaffing (Hours)"
    ) +
    jom_theme
  ggsave(file.path(plot_dir, "Fig2_Understaffing.pdf"), plot = p2, width = 8.5, height = 5)

  # Figure 3: service level.
  df_service <- df_plot %>%
    select(date, model, EV_service_level, RP_service_level) %>%
    pivot_longer(
      cols = c(EV_service_level, RP_service_level),
      names_to = "Policy",
      values_to = "ServiceLevel"
    ) %>%
    mutate(
      Policy = recode(
        Policy,
        EV_service_level = "EV (Point-based)",
        RP_service_level = "RP (Stochastic)"
      ),
      Policy = factor(Policy, levels = names(policy_colors))
    ) %>%
    group_by(model, Policy) %>%
    summarise(Mean_SL = safe_mean(ServiceLevel), .groups = "drop")

  sl_min <- max(0, min(df_service$Mean_SL, na.rm = TRUE) - 0.05)
  p3 <- ggplot(df_service, aes(x = model, y = Mean_SL, fill = Policy)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.3) +
    scale_fill_manual(values = policy_colors) +
    coord_cartesian(ylim = c(sl_min, 1.0)) +
    labs(
      title = "Service Reliability under EV and Stochastic RP",
      subtitle = "Average fraction of 15-minute slots with no realized shortage.",
      x = "Forecasting Model",
      y = "Average Service Level"
    ) +
    jom_theme
  ggsave(file.path(plot_dir, "Fig3_ServiceLevel.pdf"), plot = p3, width = 8.5, height = 5)

  # Figure 4: total realized cost.
  df_cost <- df_plot %>%
    select(date, model, EV_cost, RP_cost) %>%
    pivot_longer(
      cols = c(EV_cost, RP_cost),
      names_to = "Policy",
      values_to = "Cost"
    ) %>%
    mutate(
      Policy = recode(
        Policy,
        EV_cost = "EV (Point-based)",
        RP_cost = "RP (Stochastic)"
      ),
      Policy = factor(Policy, levels = names(policy_colors))
    )

  p4 <- ggplot(df_cost, aes(x = model, y = Cost, fill = Policy)) +
    geom_boxplot(alpha = 0.85, outlier.size = 0.5, outlier.alpha = 0.3, width = 0.7,
                 position = position_dodge(0.75)) +
    scale_fill_manual(values = policy_colors) +
    labs(
      title = "Realized Cost under EV and Stochastic RP",
      subtitle = "Daily realized cost evaluated against observed demand.",
      x = "Forecasting Model",
      y = "Realized Daily Cost"
    ) +
    jom_theme
  ggsave(file.path(plot_dir, "Fig4_TotalCost.pdf"), plot = p4, width = 8.5, height = 5)

  # Figure 5: VSS vs EVPI by model.
  df_values <- df_plot %>%
    group_by(model) %>%
    summarise(
      mean_VSS = safe_mean(VSS),
      mean_EVPI = safe_mean(EVPI),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(mean_VSS, mean_EVPI), names_to = "Metric", values_to = "Value") %>%
    mutate(
      Metric = recode(
        Metric,
        mean_VSS = "VSS (Optimization Value)",
        mean_EVPI = "EVPI (Prediction Potential)"
      ),
      Metric = factor(Metric, levels = names(value_colors))
    )

  p5 <- ggplot(df_values, aes(x = model, y = Value, fill = Metric)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.3) +
    scale_fill_manual(values = value_colors) +
    labs(
      title = "Value Decomposition: VSS versus EVPI",
      subtitle = "Average value of stochastic hedging and remaining value of perfect information.",
      x = "Forecasting Model",
      y = "Average Value"
    ) +
    jom_theme
  ggsave(file.path(plot_dir, "Fig5_VSS_vs_EVPI.pdf"), plot = p5, width = 8.5, height = 5)

  # Figure 6: decision landscape.
  df_landscape <- df_plot %>%
    group_by(model) %>%
    summarise(
      mean_VSS = safe_mean(VSS),
      mean_EVPI = safe_mean(EVPI),
      .groups = "drop"
    )

  p6 <- ggplot(df_landscape, aes(x = mean_VSS, y = mean_EVPI, label = model)) +
    geom_point(size = 5, color = "#2B2D42", alpha = 0.85) +
    ggrepel::geom_text_repel(point.padding = 0.8, size = 4.5, fontface = "bold") +
    labs(
      title = "Decision Landscape: Optimization Value versus Information Value",
      subtitle = "Forecasting models positioned by hedging value and residual uncertainty.",
      x = "Average VSS (Value of Stochastic Solution)",
      y = "Average EVPI (Expected Value of Perfect Information)"
    ) +
    jom_theme +
    theme(panel.grid.major.x = element_line(color = "gray85", linetype = "dashed", linewidth = 0.4))
  ggsave(file.path(plot_dir, "Fig6_Model_Landscape.pdf"), plot = p6, width = 6.5, height = 5.5)

  # EEV summary, exported as a benchmark table rather than as a third policy.
  eev_summary <- df_plot %>%
    group_by(model) %>%
    summarise(
      mean_EEV = safe_mean(EEV),
      mean_RP_Q = safe_mean(RP_Q),
      mean_VSS = safe_mean(VSS),
      mean_EVPI = safe_mean(EVPI),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_VSS))
  readr::write_csv(eev_summary, file.path(plot_dir, "Table_EEV_Summary.csv"))

  msg("JOM figures saved to: ", plot_dir)
  invisible(TRUE)
}

# ============================================================================
# 8. EXECUTION
# ============================================================================

msg("================ JOM forecast-to-decision reproduction ================")

if (RUN_FORECASTS) {
  msg("Step 1: loading and aggregating arrivals.")
  ae_subdaily <- load_arrivals_15min(ARRIVAL_FILE)
  msg("Aggregated slots: ", nrow(ae_subdaily), " | date range: ", min(ae_subdaily$eval_date), " to ", max(ae_subdaily$eval_date))

  forecast_data <- generate_forecast_data(
    ae_subdaily = ae_subdaily,
    target_days = TARGET_DAYS,
    train_days = TRAIN_DAYS,
    boot_times = BOOT_TIMES,
    limit_models = LIMIT_MODELS,
    output_rds = FORECAST_RDS
  )
} else {
  if (!file.exists(FORECAST_RDS)) {
    stop("Forecast RDS not found: ", FORECAST_RDS, ". Set RUN_FORECASTS <- TRUE.", call. = FALSE)
  }
  forecast_data <- readRDS(FORECAST_RDS)
}

f.con <- load_shift_matrix(SHIFT_FILE)

if (RUN_MAIN_OPTIMIZATION) {
  msg("Step 2: running baseline optimization.")
  opt_res <- run_optimization_from_forecasts(
    forecast_data = forecast_data,
    f.con = f.con,
    c_short_hourly = C_SHORT_HOURLY_BASE,
    c_over_hourly = C_OVER_HOURLY_BASE,
    c_ot_hourly = C_OT_HOURLY_BASE,
    ot_cap = OT_CAP_BASE,
    k_comp = K_COMP,
    tag = "main",
    output_main_csv = MAIN_CSV,
    output_diag_csv = DIAG_CSV,
    save_daily_curves = SAVE_DAILY_CURVES,
    evpi_method = EVPI_METHOD
  )

  main_df <- opt_res$main
  diag_df <- opt_res$diag
  summary_df <- make_model_summary(main_df, SUMMARY_CSV)
  msg("Main optimization results saved to: ", MAIN_CSV)
  msg("Diagnostic results saved to: ", DIAG_CSV)
  msg("Model summary saved to: ", SUMMARY_CSV)
  print(summary_df)
} else {
  if (!file.exists(MAIN_CSV)) {
    stop("Main optimization CSV not found: ", MAIN_CSV, ". Set RUN_MAIN_OPTIMIZATION <- TRUE.", call. = FALSE)
  }
  main_df <- readr::read_csv(MAIN_CSV, show_col_types = FALSE)
}

if (RUN_SENSITIVITY) {
  msg("Step 3: running sensitivity analysis.")
  sens_summary <- run_sensitivity_analysis(
    forecast_data = forecast_data,
    f.con = f.con,
    grid = SENSITIVITY_GRID,
    sensitivity_day_count = SENSITIVITY_DAY_COUNT
  )
  msg("Sensitivity outputs saved to: ", SENS_DIR)
  print(sens_summary)
}

if (MAKE_PLOTS) {
  msg("Step 4: generating manuscript figures.")
  if (!exists("main_df")) {
    main_df <- readr::read_csv(MAIN_CSV, show_col_types = FALSE)
  }
  make_jom_plots(main_df, PLOT_DIR)
}

msg("All requested workflow steps completed.")
