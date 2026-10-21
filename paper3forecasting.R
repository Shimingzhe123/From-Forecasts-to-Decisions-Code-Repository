# ============================================================
# SCRIPT 1: Generate Forecasts & Scenarios (Run ONCE)
# Output: "01data/forecast_data.rds"
# ============================================================

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
})

# -----------------
# 0) GLOBAL SETTINGS
# -----------------
slots_per_day        <- 96
window_size_staffreq <- 14
patient_staff_ratio  <- 0.25

# Horizon & Training
target_days <- 365     # Number of days to evaluate
train_days  <- 28
boot_times  <- 500    # Number of scenarios (draws)

# Reproducibility
set.seed(1)

# LIMIT MODELS (Optional: set to NULL to run all)
LIMIT_MODELS <- NULL 
# e.g., LIMIT_MODELS <- c("ARIMA", "LGBM")

# ============================================================
# 1) HELPERS: DATA & METRICS
# ============================================================

rolling_sum <- function(x, window_size = 14) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  vapply(seq_len(n), function(i) {
    if (i < window_size) sum(x[1:i]) else sum(x[(i - window_size + 1):i])
  }, numeric(1))
}

StaffReq_window <- function(arrivals, window_size = 14, ratio = 0.25) {
  cum <- rolling_sum(round(arrivals), window_size = window_size)
  ceiling(cum * ratio)
}

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

# ============================================================
# 2) HELPERS: FORECASTING MODELS
# ============================================================

# --- FABLE (SNAIVE, ETS, ARIMA) ---
fit_models_fable3 <- function(train_data) {
  train_ts <- train_data %>% as_tsibble(index = time)
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
  fc_tbl <- forecast(models_fit, new_data = day_block) %>%
    as_tibble() %>% select(.model, time, .mean) %>% arrange(.model, time)
  
  set.seed(seed_base)
  sims_tbl <- models_fit %>%
    generate(new_data = day_block, times = boot_times, bootstrap = TRUE) %>%
    as_tibble() %>%
    transmute(.model, time, rep = .rep, draw = pmax(0, .sim)) %>%
    arrange(.model, rep, time)
  
  list(point = fc_tbl, sims = sims_tbl)
}

# --- LIGHTGBM ---
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
  df <- train_data %>% build_lgbm_frame() %>% add_lags() %>% filter(!is.na(lag96))
  if (nrow(df) < 300) return(NULL)
  
  xy <- make_x_y(df)
  X <- xy$X; y <- xy$y
  dtrain <- lgb.Dataset(data = X, label = y)
  
  params <- list(objective = "regression", metric = "l2", learning_rate = 0.05, 
                 num_leaves = 31, min_data_in_leaf = 20, feature_fraction = 0.9, 
                 bagging_fraction = 0.9, bagging_freq = 1, verbosity = -1)
  
  booster <- lgb.train(params = params, data = dtrain, nrounds = 300, verbose = -1)
  yhat_in <- predict(booster, X)
  resid_pool <- (y - yhat_in)
  list(booster = booster, feature_names = colnames(X), form = xy$form, resid_pool = resid_pool)
}

lgbm_forecast_point_and_sims_96 <- function(model_obj, history_df, start_time, boot_times) {
  booster <- model_obj$booster; feature_names <- model_obj$feature_names
  form <- model_obj$form; resid_pool <- model_obj$resid_pool
  
  hist <- history_df %>% arrange(time) %>% mutate(visit = as.numeric(visit))
  future_times <- seq(from = start_time, by = "15 min", length.out = 96)
  
  point <- numeric(96)
  sims <- matrix(0, nrow = 96, ncol = boot_times)
  
  for (h in 1:96) {
    t_now <- future_times[h]
    tmp <- bind_rows(hist, tibble(time = t_now, visit = NA_real_)) %>% 
      build_lgbm_frame() %>% add_lags()
    row <- tmp %>% filter(time == t_now)
    
    if (any(is.na(row %>% select(lag1, lag2, lag4, lag8, lag96)))) stop("LGBM lag error")
    
    X1 <- model.matrix(form, data = row)
    X1 <- align_matrix(X1, feature_names)
    pred <- max(0, as.numeric(predict(booster, X1)))
    point[h] <- pred
    
    eps <- sample(resid_pool, size = boot_times, replace = TRUE)
    sims[h, ] <- pmax(0, pred + eps)
    hist <- bind_rows(hist, tibble(time = t_now, visit = pred))
  }
  list(point = tibble(.model="LGBM", time=future_times, .mean=point),
       sims  = tibble(.model="LGBM", time=rep(future_times, times=boot_times), 
                      rep=rep(1:boot_times, each=96), draw=as.numeric(sims)))
}

# --- GAM ---
build_gam_frame <- function(df) {
  df %>% mutate(slot = as.integer((hour(time)*60 + minute(time))/15)+1L, 
                tod = (slot-1)/96, wday_i = wday(time, week_start=1), wday_f = factor(wday_i, levels=1:7))
}

fit_gam_native <- function(train_data) {
  df <- train_data %>% build_gam_frame() %>% add_lags() %>% filter(!is.na(lag96)) %>% mutate(visit = as.numeric(visit))
  if (nrow(df) < 400) return(NULL)
  gam_fit <- mgcv::bam(visit ~ s(tod, bs="cc", k=20) + s(wday_f, bs="re") + lag1 + lag2 + lag4 + lag8 + lag96, 
                       data = df, family = gaussian(), knots = list(tod = c(0, 1)))
  list(fit = gam_fit, resid_pool = residuals(gam_fit))
}

gam_forecast_point_and_sims_96 <- function(model_obj, history_df, start_time, boot_times) {
  fit <- model_obj$fit; resid_pool <- model_obj$resid_pool
  hist <- history_df %>% arrange(time) %>% mutate(visit = as.numeric(visit))
  future_times <- seq(from = start_time, by = "15 min", length.out = 96)
  
  point <- numeric(96); sims <- matrix(0, nrow = 96, ncol = boot_times)
  for (h in 1:96) {
    t_now <- future_times[h]
    tmp <- bind_rows(hist, tibble(time = t_now, visit = NA_real_)) %>% build_gam_frame() %>% add_lags()
    row <- tmp %>% filter(time == t_now)
    pred <- max(0, as.numeric(predict(fit, newdata = row)))
    point[h] <- pred
    eps <- sample(resid_pool, size = boot_times, replace = TRUE)
    sims[h, ] <- pmax(0, pred + eps)
    hist <- bind_rows(hist, tibble(time = t_now, visit = pred))
  }
  list(point = tibble(.model="GAM", time=future_times, .mean=point),
       sims  = tibble(.model="GAM", time=rep(future_times, times=boot_times), rep=rep(1:boot_times, each=96), draw=as.numeric(sims)))
}

# --- PROPHET ---
fit_prophet_native <- function(train_data) {
  df <- train_data %>% transmute(ds = as.POSIXct(time, tz="UTC"), y = as.numeric(visit))
  if (nrow(df) < 96*7) return(NULL)
  m <- prophet::prophet(df, daily.seasonality=TRUE, weekly.seasonality=TRUE, yearly.seasonality=FALSE, seasonality.mode="additive")
  list(model = m, resid_pool = df$y - stats::predict(m, df)$yhat)
}

prophet_forecast_point_and_sims_96 <- function(model_obj, start_time, boot_times) {
  m <- model_obj$model; resid_pool <- model_obj$resid_pool
  future <- data.frame(ds = seq(from = as.POSIXct(start_time, tz="UTC"), by = "15 min", length.out = 96))
  point <- pmax(0, stats::predict(m, future)$yhat)
  sims_mat <- sapply(1:boot_times, function(j) pmax(0, point + sample(resid_pool, size=96, replace=TRUE)))
  list(point = tibble(.model="PROPHET", time=future$ds, .mean=point),
       sims  = tibble(.model="PROPHET", time=rep(future$ds, times=boot_times), rep=rep(1:boot_times, each=96), draw=as.numeric(sims_mat)))
}

# ============================================================
# 3) DATA LOADING & PREP
# ============================================================
cat("Loading data...\n")
ae_original <- readr::read_csv(
  "01data/hospital2.csv",
  col_types = cols(arrival_time = col_datetime(format = "%d/%m/%Y %H:%M"))
)
if (!"id" %in% names(ae_original)) ae_original$id <- 1

ae_subdaily <- ae_original %>%
  as_tsibble(key = id, index = arrival_time, regular = FALSE) %>%
  index_by(time = floor_date(arrival_time, "15 minutes")) %>%
  summarise(visit = n()) %>%
  fill_gaps(visit = 0) %>%
  ungroup() %>%
  mutate(time = force_tz(time, "UTC")) %>%
  arrange(time)

n_total <- nrow(ae_subdaily)
max_days_possible <- floor(n_total / slots_per_day)
horizon_days <- min(target_days, max_days_possible)
total_slots_needed <- horizon_days * slots_per_day
eval_start_idx <- n_total - total_slots_needed + 1
eval_block <- ae_subdaily[eval_start_idx:n_total, ] %>% arrange(time)

# Define models to run
model_map <- c("SNAIVE"="SNAIVE", "STL_ETS"="ETS", "ARIMA_F"="ARIMA", 
               "LGBM"="LGBM", "GAM"="GAM", "PROPHET"="Prophet")
if (!is.null(LIMIT_MODELS)) model_map <- model_map[names(model_map) %in% LIMIT_MODELS]

# ============================================================
# 4) MAIN FORECAST LOOP
# ============================================================
all_days_data <- list()
cat("Starting Forecast Generation for", horizon_days, "days...\n")

for (d in seq_len(horizon_days)) {
  cat("--- Processing Day", d, "of", horizon_days, "---\n")
  
  idx_day   <- ((d - 1) * slots_per_day + 1):(d * slots_per_day)
  day_block <- eval_block[idx_day, ] %>% arrange(time)
  planning_start <- min(day_block$time)
  
  # Ground Truth
  arrivals_actual  <- as.numeric(day_block$visit)
  staff_actual_vec <- StaffReq_window(arrivals_actual, window_size=window_size_staffreq, ratio=patient_staff_ratio)
  
  # Train Data
  train_start <- planning_start - days(train_days)
  train_data <- ae_subdaily %>% filter(time >= train_start, time < planning_start)
  
  # Fit Models
  cat("Fitting Fable models...\n")
  models_fit <- fit_models_fable3(train_data)
  out_fable_P <- predict_and_sims_fable_all(models_fit, day_block, boot_times, seed_base = 1000 + d)
  out_fable_Q <- predict_and_sims_fable_all(models_fit, day_block, boot_times, seed_base = 3000 + d)
  
  cat("Fitting Machine Learning models...\n")
  lgbm_obj <- if(nrow(train_data) >= 300) fit_lgbm_native(train_data) else NULL
  gam_obj <- fit_gam_native(train_data)
  prophet_obj <- fit_prophet_native(train_data)
  
  day_results <- list()
  day_results$info <- list(d=d, date=planning_start, actual_req=staff_actual_vec, actual_arr=arrivals_actual)
  day_results$models <- list()
  
  for (m_raw in names(model_map)) {
    m_print <- model_map[[m_raw]]
    
    # Retrieve P and Q sets
    fc_point <- NULL; sims_P <- NULL; sims_Q <- NULL
    
    if (m_raw %in% c("SNAIVE","STL_ETS","ARIMA_F")) {
      fc_point <- out_fable_P$point %>% filter(.model == m_raw)
      sims_P   <- out_fable_P$sims %>% filter(.model == m_raw) 
      sims_Q   <- out_fable_Q$sims %>% filter(.model == m_raw)
    } else if (m_raw == "LGBM" && !is.null(lgbm_obj)) {
      set.seed(1000+d+11); P <- lgbm_forecast_point_and_sims_96(lgbm_obj, ae_subdaily %>% filter(time<planning_start), planning_start, boot_times)
      set.seed(3000+d+11); Q <- lgbm_forecast_point_and_sims_96(lgbm_obj, ae_subdaily %>% filter(time<planning_start), planning_start, boot_times)
      fc_point <- P$point; sims_P <- P$sims; sims_Q <- Q$sims
    } else if (m_raw == "GAM" && !is.null(gam_obj)) {
      set.seed(1000+d+22); P <- gam_forecast_point_and_sims_96(gam_obj, ae_subdaily %>% filter(time<planning_start), planning_start, boot_times)
      set.seed(3000+d+22); Q <- gam_forecast_point_and_sims_96(gam_obj, ae_subdaily %>% filter(time<planning_start), planning_start, boot_times)
      fc_point <- P$point; sims_P <- P$sims; sims_Q <- Q$sims
    } else if (m_raw == "PROPHET" && !is.null(prophet_obj)) {
      set.seed(1000+d+33); P <- prophet_forecast_point_and_sims_96(prophet_obj, planning_start, boot_times)
      set.seed(3000+d+33); Q <- prophet_forecast_point_and_sims_96(prophet_obj, planning_start, boot_times)
      fc_point <- P$point; sims_P <- P$sims; sims_Q <- Q$sims
    }
    
    if (!is.null(fc_point)) {
      # Calc CRPS using P scenarios against actuals
      actual_df <- tibble(time = day_block$time, obs = arrivals_actual)
      crps_val  <- daily_crps_from_draws(sims_P, actual_df)
      
      # Save compressed data
      day_results$models[[m_print]] <- list(
        point = fc_point$.mean,
        sims_P_mat = matrix(sims_P$draw, nrow=96, byrow=FALSE),
        sims_Q_mat = matrix(sims_Q$draw, nrow=96, byrow=FALSE),
        crps = crps_val
      )
    }
  }
  all_days_data[[d]] <- day_results
}

cat("Saving results to 01data/forecast_data.rds ...\n")
saveRDS(all_days_data, "01data/forecast_data.rds")

cat("SUCCESS. Now run the optimization script.\n")

