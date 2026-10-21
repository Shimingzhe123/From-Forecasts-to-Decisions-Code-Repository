# ============================================================
# FULL RUNNABLE SCRIPT: Sensitivity analysis for (c_short, c_over, ot_cap)
# Optimization model: 2-stage BALANCE EQUALITY (EQ) with shortage+overstaff+overtime
#
# Outputs per grid point:
#   outputs_sens/<tag>/diag_summary.csv
#   outputs_sens/<tag>/vss_3policy_by_day_model.csv
#   outputs_sens/<tag>/plots/*.png
#   outputs_sens/<tag>/plots_vss/*.png
#
# NOTE:
# - This script assumes your data files exist:
#     01data/hospital2.csv
#     01data/shiftschedule.xlsx
# - Runs may take time. Start with small horizon_days/models if needed.
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
  library(ggplot2)
  
  library(ompr)
  library(ompr.roi)
  library(ROI)
  library(ROI.plugin.glpk)
  
  library(cluster)
  library(lightgbm)
  library(mgcv)
  library(prophet)
  library(scoringRules)
  library(ggridges)
})

# -----------------
# 0) GLOBAL SETTINGS
# -----------------
slots_per_day        <- 96
window_size_staffreq <- 14
patient_staff_ratio  <- 0.25

# evaluation horizon & training
target_days <- 7    # you can reduce (e.g., 120) for faster sensitivity runs
train_days  <- 28

# scenario settings
boot_times <- 50
k_comp     <- 15

# costs (keep c_ot fixed; we vary c_short, c_over, ot_cap)
c_ot <- 2

# solver controls
glpk_verbose         <- FALSE
glpk_time_limit_sec  <- 20

# reproducibility
set.seed(1)

# OPTIONAL: limit models for faster sensitivity (recommended initially)
# Keep as NULL to run all
LIMIT_MODELS <- NULL
# e.g. LIMIT_MODELS <- c("ARIMA", "ETS", "LGBM")

# ============================================================
# 1) Helpers: staff requirement mapping
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

# ============================================================
# 2) Probabilistic metric: CRPS (daily mean over 96 slots)
# ============================================================
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
# 3) Optimization pieces (EQUALITY BALANCE MODEL)
# ============================================================
diag_coverage <- function(x, req_vec, f.con) {
  cov <- as.vector(f.con %*% x)
  sf  <- pmax(req_vec - cov, 0)
  ov  <- pmax(cov - req_vec, 0)
  
  list(
    cov = cov,
    sf_vec = sf,
    ov_vec = ov,
    sf_area = sum(sf),
    ov_area = sum(ov),
    peak_sf_slots  = sum(sf > 0),
    peak_sf_people = max(sf),
    peak_ov_slots  = sum(ov > 0),
    peak_ov_people = max(ov)
  )
}

scenarios_to_E_mat <- function(sims_df, ratio, window_size = 14) {
  staff_by_rep <- sims_df %>%
    group_by(rep) %>%
    summarise(
      staff_vec = list(StaffReq_window(draw, window_size = window_size, ratio = ratio)),
      .groups = "drop"
    )
  do.call(cbind, staff_by_rep$staff_vec) # [T x S]
}

compress_scenarios_kmedoids <- function(E_mat, k) {
  S <- ncol(E_mat)
  if (k >= S) {
    return(list(E_comp = E_mat, p = rep(1/S, S), medoid_idx = seq_len(S), cluster = seq_len(S)))
  }
  X <- t(E_mat)
  pam_fit <- pam(X, k = k, metric = "manhattan", stand = FALSE)
  
  medoids <- pam_fit$id.med                 # length k, indices in 1..S
  cluster <- pam_fit$clustering             # length S, values 1..k
  
  # weight per cluster id
  w_by_cluster <- as.numeric(table(factor(cluster, levels = 1:k))) / S
  
  # map each medoid to its cluster id (the cluster id equals the medoid's assigned cluster)
  medoid_cluster <- cluster[medoids]        # length k, values 1..k
  p_medoids <- w_by_cluster[medoid_cluster] # aligned to medoids order
  
  E_comp <- E_mat[, medoids, drop = FALSE]
  list(E_comp = E_comp, p = p_medoids, medoid_idx = medoids, cluster = cluster)
}


# Two-stage MIP with BALANCE EQUALITY:
#   coverage(x)_t + overtime + shortage - overstaff == demand
solve_two_stage_day_from_E_balance_eq <- function(E_mat, p_s, f.con,
                                                  c_reg, c_ot, c_short, c_over,
                                                  ot_cap = NA,
                                                  verbose = FALSE,
                                                  time_limit_sec = NA) {
  n_slots  <- nrow(f.con)
  n_shifts <- ncol(f.con)
  S        <- ncol(E_mat)
  
  stopifnot(nrow(E_mat) == n_slots)
  stopifnot(length(p_s) == S)
  
  p_s <- as.numeric(p_s); p_s <- p_s / sum(p_s)
  
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n_shifts, type = "integer", lb = 0) %>%
    add_variable(o[t, s],     t = 1:n_slots, s = 1:S, lb = 0) %>%  # overtime
    add_variable(short[t, s], t = 1:n_slots, s = 1:S, lb = 0) %>%  # shortage
    add_variable(over[t, s],  t = 1:n_slots, s = 1:S, lb = 0) %>%  # overstaff
    add_constraint(
      sum_expr(f.con[t, i] * x[i], i = 1:n_shifts) + o[t, s] + short[t, s] - over[t, s] == E_mat[t, s],
      t = 1:n_slots, s = 1:S
    )
  
  if (!is.na(ot_cap)) {
    model <- model %>% add_constraint(o[t, s] <= ot_cap, t = 1:n_slots, s = 1:S)
  }
  
  model <- model %>%
    set_objective(
      sum_expr(c_reg[i] * x[i], i = 1:n_shifts) +
        sum_expr(p_s[s] * (c_ot * o[t, s] + c_short * short[t, s] + c_over * over[t, s]),
                 t = 1:n_slots, s = 1:S),
      "min"
    )
  
  control <- list()
  if (!is.na(time_limit_sec)) control$tm_limit <- as.integer(time_limit_sec * 1000)
  
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = verbose, control = control))
  
  x_sol  <- get_solution(result, x[i]) %>% arrange(i)
  o_sol  <- get_solution(result, o[t, s])
  sh_sol <- get_solution(result, short[t, s])
  ov_sol <- get_solution(result, over[t, s])
  
  ps_df <- tibble(s = 1:S, p_s = p_s)
  
  pad96 <- function(df) {
    df %>%
      left_join(ps_df, by = "s") %>%
      group_by(t) %>%
      summarise(tmp = sum(value * p_s), .groups = "drop") %>%
      right_join(tibble(t = 1:n_slots), by = "t") %>%
      mutate(tmp = replace_na(tmp, 0)) %>%
      arrange(t) %>%
      pull(tmp)
  }
  
  list(
    status = result$status,
    obj    = objective_value(result),
    x      = x_sol$value,
    exp_o_by_t     = pad96(o_sol),
    exp_short_by_t = pad96(sh_sol),
    exp_over_by_t  = pad96(ov_sol)
  )
}

eval_cost_fixed_x_vec <- function(x, req_vec, f.con, c_reg, c_ot, c_short, c_over, ot_cap = NA) {
  cov <- as.vector(f.con %*% x)
  gap <- pmax(req_vec - cov, 0)
  exc <- pmax(cov - req_vec, 0)
  
  if (is.na(ot_cap)) {
    o <- gap
  } else {
    o <- pmin(gap, ot_cap)
  }
  short <- gap - o
  over  <- exc
  
  reg_cost <- sum(c_reg * x)
  total_cost <- reg_cost + c_ot * sum(o) + c_short * sum(short) + c_over * sum(over)
  
  list(
    total_cost = total_cost,
    reg_cost   = reg_cost,
    ot_slots   = sum(o),
    sh_slots   = sum(short),
    over_slots = sum(over)
  )
}

eval_cost_fixed_x_mat <- function(x, E_mat, p_s, f.con, c_reg, c_ot, c_short, c_over, ot_cap = NA) {
  cov <- as.vector(f.con %*% x)
  S <- ncol(E_mat)
  p_s <- as.numeric(p_s); p_s <- p_s / sum(p_s)
  
  ot_s   <- numeric(S)
  sh_s   <- numeric(S)
  over_s <- numeric(S)
  
  for (s in 1:S) {
    req <- E_mat[, s]
    gap <- pmax(req - cov, 0)
    exc <- pmax(cov - req, 0)
    
    if (is.na(ot_cap)) {
      o <- gap
    } else {
      o <- pmin(gap, ot_cap)
    }
    short <- gap - o
    over  <- exc
    
    ot_s[s]   <- sum(o)
    sh_s[s]   <- sum(short)
    over_s[s] <- sum(over)
  }
  
  reg_cost <- sum(c_reg * x)
  exp_cost <- reg_cost +
    c_ot    * sum(p_s * ot_s) +
    c_short * sum(p_s * sh_s) +
    c_over  * sum(p_s * over_s)
  
  list(
    exp_cost = exp_cost,
    reg_cost = reg_cost,
    exp_ot   = sum(p_s * ot_s),
    exp_sh   = sum(p_s * sh_s),
    exp_over = sum(p_s * over_s)
  )
}

# ============================================================
# 4) Forecast models
# ============================================================
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

# ---- LGBM helpers ----
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
    build_lgbm_frame() %>%
    add_lags() %>%
    filter(!is.na(lag96))
  
  if (nrow(df) < 300) return(NULL)
  
  xy <- make_x_y(df)
  X <- xy$X
  y <- xy$y
  feature_names <- colnames(X)
  
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
  resid_pool <- (y - yhat_in)
  
  list(booster = booster, feature_names = feature_names, form = xy$form, resid_pool = resid_pool)
}

lgbm_forecast_point_and_sims_96 <- function(model_obj, history_df, start_time, boot_times) {
  booster <- model_obj$booster
  feature_names <- model_obj$feature_names
  form <- model_obj$form
  resid_pool <- model_obj$resid_pool
  
  hist <- history_df %>%
    arrange(time) %>%
    mutate(visit = as.numeric(visit))
  
  future_times <- seq(from = start_time, by = "15 min", length.out = 96)
  
  point <- numeric(96)
  sims <- matrix(0, nrow = 96, ncol = boot_times)
  
  for (h in 1:96) {
    t_now <- future_times[h]
    
    tmp <- bind_rows(hist, tibble(time = t_now, visit = NA_real_)) %>%
      build_lgbm_frame() %>%
      add_lags()
    
    row <- tmp %>% filter(time == t_now)
    
    if (any(is.na(row %>% select(lag1, lag2, lag4, lag8, lag96)))) {
      stop("LGBM: lag features NA at horizon step ", h, ". Not enough history.")
    }
    
    X1 <- model.matrix(form, data = row)
    X1 <- align_matrix(X1, feature_names)
    
    pred <- as.numeric(predict(booster, X1))
    pred <- max(0, pred)
    point[h] <- pred
    
    eps <- sample(resid_pool, size = boot_times, replace = TRUE)
    sims[h, ] <- pmax(0, pred + eps)
    
    hist <- bind_rows(hist, tibble(time = t_now, visit = pred))
  }
  
  list(
    point = tibble(.model = "LGBM", time = future_times, .mean = point),
    sims  = tibble(
      .model = "LGBM",
      time   = rep(future_times, times = boot_times),
      rep    = rep(1:boot_times, each = 96),
      draw   = as.numeric(sims)
    )
  )
}

# ---- GAM helpers ----
build_gam_frame <- function(df) {
  df %>%
    mutate(
      slot   = as.integer((hour(time) * 60 + minute(time)) / 15) + 1L,
      tod    = (slot - 1) / 96,
      wday_i = wday(time, week_start = 1),
      wday_f = factor(wday_i, levels = 1:7)
    )
}

fit_gam_native <- function(train_data) {
  df <- train_data %>%
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
  
  resid_pool <- residuals(gam_fit)
  list(fit = gam_fit, resid_pool = resid_pool)
}

gam_forecast_point_and_sims_96 <- function(model_obj, history_df, start_time, boot_times) {
  fit <- model_obj$fit
  resid_pool <- model_obj$resid_pool
  
  hist <- history_df %>%
    arrange(time) %>%
    mutate(visit = as.numeric(visit))
  
  future_times <- seq(from = start_time, by = "15 min", length.out = 96)
  point <- numeric(96)
  sims  <- matrix(0, nrow = 96, ncol = boot_times)
  
  for (h in 1:96) {
    t_now <- future_times[h]
    
    tmp <- bind_rows(hist, tibble(time = t_now, visit = NA_real_)) %>%
      build_gam_frame() %>%
      add_lags()
    
    row <- tmp %>% filter(time == t_now)
    
    if (any(is.na(row %>% select(lag1, lag2, lag4, lag8, lag96)))) {
      stop("GAM: lag features NA at horizon step ", h, ". Not enough history.")
    }
    
    pred <- as.numeric(predict(fit, newdata = row))
    pred <- max(0, pred)
    point[h] <- pred
    
    eps <- sample(resid_pool, size = boot_times, replace = TRUE)
    sims[h, ] <- pmax(0, pred + eps)
    
    hist <- bind_rows(hist, tibble(time = t_now, visit = pred))
  }
  
  list(
    point = tibble(.model = "GAM", time = future_times, .mean = point),
    sims  = tibble(
      .model = "GAM",
      time   = rep(future_times, times = boot_times),
      rep    = rep(1:boot_times, each = 96),
      draw   = as.numeric(sims)
    )
  )
}

# ---- Prophet helpers ----
fit_prophet_native <- function(train_data) {
  df <- train_data %>%
    transmute(ds = as.POSIXct(time, tz = "UTC"), y = as.numeric(visit))
  
  if (nrow(df) < 96 * 7) return(NULL)
  
  m <- prophet::prophet(
    df,
    daily.seasonality  = TRUE,
    weekly.seasonality = TRUE,
    yearly.seasonality = FALSE,
    seasonality.mode   = "additive"
  )
  
  pred_in <- stats::predict(m, df)
  resid_pool <- df$y - pred_in$yhat
  
  list(model = m, resid_pool = resid_pool)
}

prophet_forecast_point_and_sims_96 <- function(model_obj, start_time, boot_times) {
  m <- model_obj$model
  resid_pool <- model_obj$resid_pool
  
  future <- data.frame(ds = seq(from = as.POSIXct(start_time, tz = "UTC"), by = "15 min", length.out = 96))
  pred   <- stats::predict(m, future)
  point  <- pmax(0, pred$yhat)
  
  sims_mat <- sapply(
    1:boot_times,
    function(j) pmax(0, point + sample(resid_pool, size = 96, replace = TRUE))
  )
  
  list(
    point = tibble(.model = "PROPHET", time = future$ds, .mean = point),
    sims  = tibble(
      .model = "PROPHET",
      time   = rep(future$ds, times = boot_times),
      rep    = rep(1:boot_times, each = 96),
      draw   = as.numeric(sims_mat)
    )
  )
}

# ============================================================
# 5) Load & wrangle data ONCE (shared across runs)
# ============================================================
ae_original <- readr::read_csv(
  "01data/hospital2.csv",
  col_types = cols(arrival_time = col_datetime(format = "%d/%m/%Y %H:%M"))
)
if (!"id" %in% names(ae_original)) ae_original$id <- 1

ae_tsb <- ae_original %>%
  as_tsibble(key = id, index = arrival_time, regular = FALSE)

ae_subdaily <- ae_tsb %>%
  index_by(time = floor_date(arrival_time, "15 minutes")) %>%
  summarise(visit = n()) %>%
  fill_gaps(visit = 0) %>%
  ungroup() %>%
  mutate(time = force_tz(time, "UTC")) %>%
  arrange(time)

n_total <- nrow(ae_subdaily)
if (n_total < slots_per_day * 10) stop("Not enough data (need at least ~10 full days for lags).")

# Coverage matrix f.con
shiftpattern <- read_xlsx("01data/shiftschedule.xlsx")
shift_cols   <- grep("^Shift", names(shiftpattern), value = TRUE)
stopifnot(length(shift_cols) > 0)

f.con <- shiftpattern %>% select(all_of(shift_cols)) %>% as.matrix()
if (nrow(f.con) != slots_per_day) stop("Coverage matrix must have 96 rows.")

shift_number <- ncol(f.con)

# ---- Regular cost: charge by shift covered hours (consistent with main script)
# Each slot = 15 min => 0.25 hour
slot_hours <- 24 / slots_per_day   # = 0.25 when slots_per_day=96

# f.con expected to be 0/1 coverage. If it contains other weights, treat >0 as covered.
covered_slots <- colSums(f.con > 0)
covered_hours <- covered_slots * slot_hours

# Set your regular cost per hour (use 1 if you just want "cost proportional to hours")
reg_cost_per_hour <- 1

# c_reg[i] = regular cost of staffing one person on shift i
c_reg <- reg_cost_per_hour * covered_hours

# sanity checks / quick print
stopifnot(length(c_reg) == shift_number)
cat("Regular cost by shift (hours):\n")
print(round(covered_hours, 2))
cat("Regular cost by shift (c_reg):\n")
print(round(c_reg, 2))


# Eval block: last target_days
max_days_possible <- floor(n_total / slots_per_day)
horizon_days <- min(target_days, max_days_possible)
total_slots_needed <- horizon_days * slots_per_day

eval_start_idx <- n_total - total_slots_needed + 1
eval_end_idx   <- n_total

eval_block <- ae_subdaily[eval_start_idx:eval_end_idx, ] %>% arrange(time)
cat("Using", horizon_days, "days, rows", eval_start_idx, "to", eval_end_idx, "\n")

# ============================================================
# 6) One run (one parameter setting)
# ============================================================
run_experiment <- function(c_short, c_over, ot_cap, tag,
                           horizon_days_override = NULL,
                           limit_models = NULL) {
  
  # directories per run
  base_dir <- file.path("outputs_sens", tag)
  dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(base_dir, "daily_curves"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(base_dir, "x_solutions"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(base_dir, "plots"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(base_dir, "plots_vss"), showWarnings = FALSE, recursive = TRUE)
  
  cat("\n=================================================\n")
  cat("RUN TAG:", tag, "\n")
  cat("Params: c_short =", c_short, "| c_over =", c_over, "| ot_cap =", ifelse(is.na(ot_cap), "NA", ot_cap), "\n")
  cat("=================================================\n")
  
  # model map (internal id -> display)
  model_map <- c(
    "SNAIVE"   = "SNAIVE",
    "STL_ETS"  = "ETS",
    "ARIMA_F"  = "ARIMA",
    "LGBM"     = "LGBM",
    "GAM"      = "GAM",
    "PROPHET"  = "Prophet"
  )
  
  # optional model filtering by display names
  if (!is.null(limit_models)) {
    keep_raw <- names(model_map)[model_map %in% limit_models]
    model_map <- model_map[keep_raw]
  }
  
  hd <- horizon_days
  if (!is.null(horizon_days_override)) hd <- min(horizon_days_override, horizon_days)
  
  results_diag <- list()
  results_vss3 <- list()
  row_k <- 0
  row_v <- 0
  
  for (d in seq_len(hd)) {
    
    idx_day   <- ((d - 1) * slots_per_day + 1):(d * slots_per_day)
    day_block <- eval_block[idx_day, ] %>% arrange(time)
    
    planning_start <- min(day_block$time)
    planning_date  <- as.Date(planning_start)
    
    arrivals_actual  <- as.numeric(day_block$visit)
    staff_actual_vec <- StaffReq_window(arrivals_actual, window_size = window_size_staffreq, ratio = patient_staff_ratio)
    
    # training window
    train_start <- planning_start - days(train_days)
    train_data <- ae_subdaily %>% filter(time >= train_start, time < planning_start)
    
    # fable P and Q independent
    models_fit <- fit_models_fable3(train_data)
    out_fable_P <- predict_and_sims_fable_all(models_fit, day_block, boot_times, seed_base = 1000 + d)
    out_fable_Q <- predict_and_sims_fable_all(models_fit, day_block, boot_times, seed_base = 3000 + d)
    
    # LGBM fit once per day
    lgbm_obj <- NULL
    if (nrow(train_data) >= (96 + 120)) lgbm_obj <- fit_lgbm_native(train_data)
    
    # GAM fit once per day
    gam_obj <- fit_gam_native(train_data)
    
    # Prophet fit once per day
    prophet_obj <- fit_prophet_native(train_data)
    
    for (m_raw in names(model_map)) {
      m_print <- model_map[[m_raw]]
      
      # Get P: point + sims_P (arrivals); Q: sims_Q independent
      if (m_raw %in% c("SNAIVE","STL_ETS","ARIMA_F")) {
        
        fc_m <- out_fable_P$point %>% filter(.model == m_raw) %>% arrange(time)
        sims_P <- out_fable_P$sims %>% filter(.model == m_raw) %>% select(time, rep, draw) %>% arrange(rep, time)
        sims_Q <- out_fable_Q$sims %>% filter(.model == m_raw) %>% select(time, rep, draw) %>% arrange(rep, time)
        
      } else if (m_raw == "LGBM") {
        
        if (is.null(lgbm_obj)) { next }
        
        set.seed(1000 + d + 11)
        out_lgbm_P <- lgbm_forecast_point_and_sims_96(
          model_obj  = lgbm_obj,
          history_df = ae_subdaily %>% filter(time < planning_start),
          start_time = planning_start,
          boot_times = boot_times
        )
        
        set.seed(3000 + d + 11)
        out_lgbm_Q <- lgbm_forecast_point_and_sims_96(
          model_obj  = lgbm_obj,
          history_df = ae_subdaily %>% filter(time < planning_start),
          start_time = planning_start,
          boot_times = boot_times
        )
        
        fc_m   <- out_lgbm_P$point %>% arrange(time)
        sims_P <- out_lgbm_P$sims  %>% select(time, rep, draw) %>% arrange(rep, time)
        sims_Q <- out_lgbm_Q$sims  %>% select(time, rep, draw) %>% arrange(rep, time)
        
      } else if (m_raw == "GAM") {
        
        if (is.null(gam_obj)) { next }
        
        set.seed(1000 + d + 22)
        out_gam_P <- gam_forecast_point_and_sims_96(
          model_obj  = gam_obj,
          history_df = ae_subdaily %>% filter(time < planning_start),
          start_time = planning_start,
          boot_times = boot_times
        )
        
        set.seed(3000 + d + 22)
        out_gam_Q <- gam_forecast_point_and_sims_96(
          model_obj  = gam_obj,
          history_df = ae_subdaily %>% filter(time < planning_start),
          start_time = planning_start,
          boot_times = boot_times
        )
        
        fc_m   <- out_gam_P$point %>% arrange(time)
        sims_P <- out_gam_P$sims  %>% select(time, rep, draw) %>% arrange(rep, time)
        sims_Q <- out_gam_Q$sims  %>% select(time, rep, draw) %>% arrange(rep, time)
        
      } else if (m_raw == "PROPHET") {
        
        if (is.null(prophet_obj)) { next }
        
        set.seed(1000 + d + 33)
        out_pr_P <- prophet_forecast_point_and_sims_96(prophet_obj, planning_start, boot_times)
        
        set.seed(3000 + d + 33)
        out_pr_Q <- prophet_forecast_point_and_sims_96(prophet_obj, planning_start, boot_times)
        
        fc_m   <- out_pr_P$point %>% arrange(time)
        sims_P <- out_pr_P$sims  %>% select(time, rep, draw) %>% arrange(rep, time)
        sims_Q <- out_pr_Q$sims  %>% select(time, rep, draw) %>% arrange(rep, time)
        
      } else next
      
      # CRPS (based on P draws)
      actual_df <- tibble(time = day_block$time, obs = arrivals_actual)
      crps_day  <- daily_crps_from_draws(sims_P, actual_df)
      
      # Build req from point forecast
      yhat <- pmax(0, as.numeric(fc_m$.mean))
      req_point <- StaffReq_window(yhat, window_size = window_size_staffreq, ratio = patient_staff_ratio)
      
      # ----------------------------
      # POLICY 1: Perfect-info (EQ, single scenario = actual req)
      # ----------------------------
      E_perf <- matrix(staff_actual_vec, ncol = 1)
      perf_res <- solve_two_stage_day_from_E_balance_eq(
        E_mat = E_perf, p_s = 1,
        f.con = f.con, c_reg = c_reg,
        c_ot = c_ot, c_short = c_short, c_over = c_over,
        ot_cap = ot_cap,
        verbose = glpk_verbose,
        time_limit_sec = glpk_time_limit_sec
      )
      x_perf <- perf_res$x
      cost_perf_real <- eval_cost_fixed_x_vec(x_perf, staff_actual_vec, f.con, c_reg, c_ot, c_short, c_over, ot_cap)
      
      # ----------------------------
      # POLICY 2: EV (EQ, single scenario = req_point)
      # ----------------------------
      E_ev <- matrix(req_point, ncol = 1)
      ev_res <- solve_two_stage_day_from_E_balance_eq(
        E_mat = E_ev, p_s = 1,
        f.con = f.con, c_reg = c_reg,
        c_ot = c_ot, c_short = c_short, c_over = c_over,
        ot_cap = ot_cap,
        verbose = glpk_verbose,
        time_limit_sec = glpk_time_limit_sec
      )
      x_ev <- ev_res$x
      cost_ev_real <- eval_cost_fixed_x_vec(x_ev, staff_actual_vec, f.con, c_reg, c_ot, c_short, c_over, ot_cap)
      diag_ev <- diag_coverage(x_ev, staff_actual_vec, f.con)
      
      # ----------------------------
      # POLICY 3: RP (EQ, compressed P scenarios)
      # ----------------------------
      E_raw_P <- scenarios_to_E_mat(sims_P, ratio = patient_staff_ratio, window_size = window_size_staffreq)
      compP <- compress_scenarios_kmedoids(E_raw_P, k = k_comp)
      E_cP  <- compP$E_comp
      p_cP  <- compP$p
      
      rp_res <- solve_two_stage_day_from_E_balance_eq(
        E_mat = E_cP, p_s = p_cP,
        f.con = f.con, c_reg = c_reg,
        c_ot = c_ot, c_short = c_short, c_over = c_over,
        ot_cap = ot_cap,
        verbose = glpk_verbose,
        time_limit_sec = glpk_time_limit_sec
      )
      x_rp <- rp_res$x
      cost_rp_real <- eval_cost_fixed_x_vec(x_rp, staff_actual_vec, f.con, c_reg, c_ot, c_short, c_over, ot_cap)
      diag_rp <- diag_coverage(x_rp, staff_actual_vec, f.con)
      
      # Export daily curves
      curve_df <- tibble(
        day_index = d,
        date      = planning_date,
        model     = m_print,
        t         = 1:slots_per_day,
        time      = day_block$time,
        actual_arrivals = arrivals_actual,
        actual_req      = staff_actual_vec,
        Point_req       = req_point,
        EV_coverage  = diag_ev$cov,
        EV_shortfall = diag_ev$sf_vec,
        EV_overstaff = diag_ev$ov_vec,
        RP_coverage  = diag_rp$cov,
        RP_shortfall = diag_rp$sf_vec,
        RP_overstaff = diag_rp$ov_vec,
        RP_exp_overtime  = rp_res$exp_o_by_t,
        RP_exp_shortage  = rp_res$exp_short_by_t,
        RP_exp_overstaff = rp_res$exp_over_by_t
      )
      readr::write_csv(curve_df, file.path(base_dir, "daily_curves", paste0("curves_day", d, "_", m_print, ".csv")))
      
      # Export x decisions
      x_df <- tibble(
        day_index = d,
        date      = planning_date,
        model     = m_print,
        shift_id  = 1:shift_number,
        x_perf    = x_perf,
        x_EV      = x_ev,
        x_RP      = x_rp
      )
      readr::write_csv(x_df, file.path(base_dir, "x_solutions", paste0("x_day", d, "_", m_print, ".csv")))
      
      # Diagnostics summary
      row_k <- row_k + 1
      results_diag[[row_k]] <- tibble(
        day_index = d,
        date      = planning_date,
        model     = m_print,
        CRPS_day  = crps_day,
        EV_under_hours = diag_ev$sf_area * 0.25,
        EV_over_hours  = diag_ev$ov_area * 0.25,
        RP_under_hours = diag_rp$sf_area * 0.25,
        RP_over_hours  = diag_rp$ov_area * 0.25,
        RP_exp_ot_hours   = sum(rp_res$exp_o_by_t) * 0.25,
        RP_exp_sh_hours   = sum(rp_res$exp_short_by_t) * 0.25,
        RP_exp_over_hours = sum(rp_res$exp_over_by_t) * 0.25,
        status_perf = as.character(perf_res$status),
        status_EV   = as.character(ev_res$status),
        status_RP   = as.character(rp_res$status),
        # record sensitivity params
        c_short = c_short,
        c_over  = c_over,
        ot_cap  = ot_cap
      )
      
      # VSS under independent Q
      E_raw_Q <- scenarios_to_E_mat(sims_Q, ratio = patient_staff_ratio, window_size = window_size_staffreq)
      p_Q <- rep(1 / ncol(E_raw_Q), ncol(E_raw_Q))
      
      EEV_Q <- eval_cost_fixed_x_mat(x_ev, E_raw_Q, p_Q, f.con, c_reg, c_ot, c_short, c_over, ot_cap)
      RP_Q  <- eval_cost_fixed_x_mat(x_rp, E_raw_Q, p_Q, f.con, c_reg, c_ot, c_short, c_over, ot_cap)
      VSS_Q <- EEV_Q$exp_cost - RP_Q$exp_cost
      
      row_v <- row_v + 1
      results_vss3[[row_v]] <- tibble(
        day_index = d,
        date      = planning_date,
        model     = m_print,
        cost_perf_real = cost_perf_real$total_cost,
        cost_ev_real   = cost_ev_real$total_cost,
        cost_rp_real   = cost_rp_real$total_cost,
        EEV_cost_Q = EEV_Q$exp_cost,
        RP_cost_Q  = RP_Q$exp_cost,
        VSS_Q      = VSS_Q,
        status_perf = as.character(perf_res$status),
        status_EV   = as.character(ev_res$status),
        status_RP   = as.character(rp_res$status),
        # record sensitivity params
        c_short = c_short,
        c_over  = c_over,
        ot_cap  = ot_cap
      )
      
      cat(sprintf(
        "Tag=%s | Day %d | %-7s | CRPS=%.3f | EV_under=%.2f | RP_under=%.2f | VSS(Q)=%.2f | ot_cap=%s\n",
        tag, d, m_print, crps_day,
        diag_ev$sf_area * 0.25,
        diag_rp$sf_area * 0.25,
        VSS_Q,
        ifelse(is.na(ot_cap), "NA", as.character(ot_cap))
      ))
    }
  }
  
  # Save CSV
  diag_df <- bind_rows(results_diag) %>% arrange(day_index, model)
  vss_df  <- bind_rows(results_vss3) %>% arrange(day_index, model)
  
  readr::write_csv(diag_df, file.path(base_dir, "diag_summary.csv"))
  readr::write_csv(vss_df,  file.path(base_dir, "vss_3policy_by_day_model.csv"))
  
  # Quick plots
  p_crps <- diag_df %>%
    ggplot(aes(x = date, y = CRPS_day, color = model)) +
    geom_line() + geom_point() +
    labs(title = paste0("CRPS (tag=", tag, ")"), y = "CRPS", x = "date") +
    theme_bw()
  ggsave(file.path(base_dir, "plots", "CRPS_by_model.png"), p_crps, width = 9, height = 4.5, dpi = 300)
  
  p_under <- diag_df %>%
    ggplot(aes(x = date, y = EV_under_hours, color = model)) +
    geom_line() + geom_point() +
    labs(title = paste0("EV realised understaff (hours) (tag=", tag, ")"), y = "hours", x = "date") +
    theme_bw()
  ggsave(file.path(base_dir, "plots", "EV_understaff_hours_by_model.png"), p_under, width = 9, height = 4.5, dpi = 300)
  
  p_rp_under <- diag_df %>%
    ggplot(aes(x = date, y = RP_under_hours, color = model)) +
    geom_line() + geom_point() +
    labs(title = paste0("RP realised understaff (hours) (tag=", tag, ")"), y = "hours", x = "date") +
    theme_bw()
  ggsave(file.path(base_dir, "plots", "RP_understaff_hours_by_model.png"), p_rp_under, width = 9, height = 4.5, dpi = 300)
  
  # Ridgelines
  cost_long_ridge <- vss_df %>%
    select(date, model, cost_perf_real, cost_ev_real, cost_rp_real) %>%
    pivot_longer(cols = -c(date, model), names_to = "policy", values_to = "cost") %>%
    mutate(
      policy = recode(policy,
                      cost_perf_real  = "Perfect-info (2-stage, EQ)",
                      cost_ev_real    = "EV (Point, 2-stage, EQ)",
                      cost_rp_real    = "RP (Stochastic, 2-stage, EQ)")
    ) %>%
    filter(is.finite(cost))
  
  p_ridge_cost <- ggplot(cost_long_ridge, aes(x = cost, y = policy, fill = policy)) +
    geom_density_ridges(scale = 1.1, alpha = 0.85, color = "white", linewidth = 0.25) +
    facet_wrap(~ model, scales = "free_x") +
    labs(title = paste0("Realised daily cost by policy (tag=", tag, ")"),
         x = "Realised daily cost (actual)", y = NULL) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(file.path(base_dir, "plots_vss", "ridgeline_realised_cost_by_policy_faceted_model.png"),
         p_ridge_cost, width = 12, height = 7, dpi = 300)
  
  vss_ridge <- vss_df %>% filter(is.finite(VSS_Q))
  p_ridge_vss <- ggplot(vss_ridge, aes(x = VSS_Q, y = model, fill = model)) +
    geom_density_ridges(scale = 1.2, alpha = 0.85, color = "white", linewidth = 0.25) +
    labs(title = paste0("VSS distribution by model (tag=", tag, ")"),
         x = "VSS(Q) = E_Q[cost(x_EV)] - E_Q[cost(x_RP)]", y = NULL) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(file.path(base_dir, "plots_vss", "ridgeline_VSS_by_model.png"),
         p_ridge_vss, width = 10, height = 5.5, dpi = 300)
  
  invisible(list(diag_df = diag_df, vss_df = vss_df))
}

# ============================================================
# 7) SENSITIVITY GRID + RUN
# ============================================================

grid <- expand.grid(
  c_short = c(5, 10, 20),
  c_over  = c(0, 0.2, 1),
  ot_cap  = c(1, 2, NA)
)

# OPTIONAL: for faster pilot sensitivity, override to e.g. 120 days
HORIZON_OVERRIDE <- NULL
# HORIZON_OVERRIDE <- 120

for (i in 1:nrow(grid)) {
  p <- grid[i, ]
  
  tag <- paste0(
    "cs", p$c_short,
    "_co", p$c_over,
    "_ot", ifelse(is.na(p$ot_cap), "NA", p$ot_cap)
  )
  
  run_experiment(
    c_short = p$c_short,
    c_over  = p$c_over,
    ot_cap  = p$ot_cap,
    tag     = tag,
    horizon_days_override = HORIZON_OVERRIDE,
    limit_models = LIMIT_MODELS
  )
}

cat("\nAll sensitivity runs finished.\n")
cat("Check outputs under: outputs_sens/\n")

# ============================================================
# 8) (Optional) AFTER ALL RUNS: Build a single summary table
# ============================================================

summarise_all_runs <- function(outputs_root = "outputs_sens") {
  run_dirs <- list.dirs(outputs_root, recursive = FALSE, full.names = TRUE)
  csvs <- file.path(run_dirs, "vss_3policy_by_day_model.csv")
  csvs <- csvs[file.exists(csvs)]
  if (length(csvs) == 0) stop("No run CSVs found in ", outputs_root)
  
  all <- purrr::map_dfr(csvs, function(f) {
    df <- readr::read_csv(f, show_col_types = FALSE)
    df$run_tag <- basename(dirname(f))
    df
  })
  
  summary <- all %>%
    group_by(run_tag, c_short, c_over, ot_cap, model) %>%
    summarise(
      mean_VSS = mean(VSS_Q, na.rm = TRUE),
      median_VSS = median(VSS_Q, na.rm = TRUE),
      p90_VSS = quantile(VSS_Q, 0.90, na.rm = TRUE),
      mean_EEV = mean(EEV_cost_Q, na.rm = TRUE),
      mean_RP  = mean(RP_cost_Q, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(model, c_short, c_over, ot_cap)
  
  readr::write_csv(summary, file.path(outputs_root, "sensitivity_summary_by_model.csv"))
  summary
}

# Uncomment to create the summary CSV after all runs:
sens_summary <- summarise_all_runs()
print(sens_summary)


write_csv(
  sens_summary,
  "outputs_sens/sensitivity_summary_by_modelall.csv"
)