# ============================================================
# SCRIPT 2: Optimization Sensitivity Analysis (Unit-Consistent Version)
# FIXED: Added missing 'verbose' argument parameter
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(purrr)
  library(ompr)
  library(ompr.roi)
  library(ROI)
  library(ROI.plugin.glpk)
  library(cluster)
  library(ggplot2)
})

# -----------------
# 1) GLOBAL SETTINGS
# -----------------
slots_per_day  <- 96
slot_minutes   <- 15
slots_per_hour <- 60 / slot_minutes # = 4

window_size_staffreq <- 14
patient_staff_ratio  <- 0.25
k_comp <- 20

# Solver settings
glpk_verbose <- FALSE
glpk_time_limit_sec <- 20

# --- Helper Functions ---
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

compress_scenarios_kmedoids <- function(E_mat, k) {
  S <- ncol(E_mat)
  if (k >= S) return(list(E_comp = E_mat, p = rep(1/S, S)))
  pam_fit <- pam(t(E_mat), k = k, metric = "manhattan", stand = FALSE)
  medoids <- pam_fit$id.med
  cluster <- pam_fit$clustering
  w_by_cluster <- as.numeric(table(factor(cluster, levels = 1:k))) / S
  p_medoids <- w_by_cluster[cluster[medoids]]
  list(E_comp = E_mat[, medoids, drop = FALSE], p = p_medoids)
}

# --- FIXED FUNCTION BELOW (Added verbose = FALSE) ---
solve_two_stage_day_from_E_balance_eq <- function(E_mat, p_s, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap = NA, verbose = FALSE) {
  n_slots <- nrow(f.con); n_shifts <- ncol(f.con); S <- ncol(E_mat)
  p_s <- as.numeric(p_s) / sum(p_s)
  
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n_shifts, type = "integer", lb = 0) %>%
    add_variable(o[t, s], t = 1:n_slots, s = 1:S, lb = 0) %>%
    add_variable(short[t, s], t = 1:n_slots, s = 1:S, lb = 0) %>%
    add_variable(over[t, s], t = 1:n_slots, s = 1:S, lb = 0) %>%
    add_constraint(
      sum_expr(f.con[t, i] * x[i], i = 1:n_shifts) + o[t, s] + short[t, s] - over[t, s] == E_mat[t, s],
      t = 1:n_slots, s = 1:S
    ) %>%
    set_objective(
      sum_expr(c_reg[i] * x[i], i = 1:n_shifts) +
        sum_expr(p_s[s] * (c_ot_slot * o[t, s] + c_short_slot * short[t, s] + c_over_slot * over[t, s]), t = 1:n_slots, s = 1:S),
      "min"
    )
  
  if (!is.na(ot_cap)) model <- model %>% add_constraint(o[t, s] <= ot_cap, t = 1:n_slots, s = 1:S)
  
  control <- list(); if (glpk_time_limit_sec > 0) control$tm_limit <- glpk_time_limit_sec * 1000
  
  # Now 'verbose' is correctly defined from the argument
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = verbose, control = control))
  
  list(x = get_solution(result, x[i]) %>% arrange(i) %>% pull(value), status = result$status)
}

eval_cost_fixed_x_vec <- function(x, req_vec, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap = NA) {
  cov <- as.vector(f.con %*% x)
  gap <- pmax(req_vec - cov, 0)
  exc <- pmax(cov - req_vec, 0)
  o <- if (is.na(ot_cap)) gap else pmin(gap, ot_cap)
  short <- gap - o
  over  <- exc
  total_cost <- sum(c_reg * x) + c_ot_slot * sum(o) + c_short_slot * sum(short) + c_over_slot * sum(over)
  list(total_cost = total_cost)
}

eval_cost_fixed_x_mat <- function(x, E_mat, p_s, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap = NA) {
  cov <- as.vector(f.con %*% x)
  S <- ncol(E_mat); p_s <- as.numeric(p_s) / sum(p_s)
  costs <- numeric(S)
  for (s in 1:S) {
    req <- E_mat[, s]
    gap <- pmax(req - cov, 0)
    exc <- pmax(cov - req, 0)
    o <- if (is.na(ot_cap)) gap else pmin(gap, ot_cap)
    short <- gap - o
    over  <- exc
    costs[s] <- c_ot_slot * sum(o) + c_short_slot * sum(short) + c_over_slot * sum(over)
  }
  exp_cost <- sum(c_reg * x) + sum(p_s * costs)
  list(exp_cost = exp_cost)
}

# -----------------
# 2) LOAD DATA
# -----------------
cat("Loading Forecast Data from RDS...\n")
if (!file.exists("01data/forecast_data.rds")) stop("Run script 01_generate_forecasts.R first!")
forecast_data <- readRDS("01data/forecast_data.rds")

shiftpattern <- read_xlsx("01data/shiftschedule.xlsx")
shift_cols   <- grep("^Shift", names(shiftpattern), value = TRUE)
f.con <- shiftpattern %>% select(all_of(shift_cols)) %>% as.matrix()

# --- Unit Consistent Regular Cost ---
# Base unit: regular cost of 1 nurse for 1 hour = 1.0
reg_cost_per_hour <- 1.0
covered_hours <- colSums(f.con) * (slot_minutes / 60) # Actual hours per shift
c_reg <- reg_cost_per_hour * covered_hours

# -----------------
# 3) GRID SEARCH RUNNER (HOURLY INPUTS)
# -----------------
run_sensitivity_grid <- function(c_short_hourly, c_over_hourly, ot_cap, tag) {
  
  # --- CONVERT HOURLY TO SLOT COSTS HERE ---
  # Overtime hourly rate fixed at 1.5
  c_ot_hourly <- 1.5
  
  c_ot_slot    <- c_ot_hourly / slots_per_hour
  c_short_slot <- c_short_hourly / slots_per_hour
  c_over_slot  <- c_over_hourly / slots_per_hour
  
  cat(sprintf("Running Tag: %s | Hourly: Short=%.1f, Over=%.1f | Slot: Short=%.2f, Over=%.2f\n", 
              tag, c_short_hourly, c_over_hourly, c_short_slot, c_over_slot))
  
  base_dir <- file.path("outputs_sens", tag)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  
  results_vss <- list()
  row_v <- 0
  
  for (day_obj in forecast_data) {
    d <- day_obj$info$d
    planning_date <- day_obj$info$date
    staff_actual_vec <- day_obj$info$actual_req
    
    for (m_print in names(day_obj$models)) {
      m_data <- day_obj$models[[m_print]]
      
      point_req <- StaffReq_window(pmax(0, m_data$point), window_size_staffreq, patient_staff_ratio)
      sims_P_req <- apply(m_data$sims_P_mat, 2, StaffReq_window, window_size=window_size_staffreq, ratio=patient_staff_ratio)
      sims_Q_req <- apply(m_data$sims_Q_mat, 2, StaffReq_window, window_size=window_size_staffreq, ratio=patient_staff_ratio)
      
      # 1. Perfect Info
      E_perf <- matrix(staff_actual_vec, ncol = 1)
      perf_res <- solve_two_stage_day_from_E_balance_eq(E_perf, 1, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap, verbose=glpk_verbose)
      cost_perf <- eval_cost_fixed_x_vec(perf_res$x, staff_actual_vec, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap)
      
      # 2. EV
      E_ev <- matrix(point_req, ncol = 1)
      ev_res <- solve_two_stage_day_from_E_balance_eq(E_ev, 1, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap, verbose=glpk_verbose)
      cost_ev_real <- eval_cost_fixed_x_vec(ev_res$x, staff_actual_vec, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap)
      
      # 3. RP
      compP <- compress_scenarios_kmedoids(sims_P_req, k = k_comp)
      rp_res <- solve_two_stage_day_from_E_balance_eq(compP$E_comp, compP$p, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap, verbose=glpk_verbose)
      cost_rp_real <- eval_cost_fixed_x_vec(rp_res$x, staff_actual_vec, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap)
      
      # 4. VSS
      p_Q <- rep(1/ncol(sims_Q_req), ncol(sims_Q_req))
      EEV_Q <- eval_cost_fixed_x_mat(ev_res$x, sims_Q_req, p_Q, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap)
      RP_Q  <- eval_cost_fixed_x_mat(rp_res$x, sims_Q_req, p_Q, f.con, c_reg, c_ot_slot, c_short_slot, c_over_slot, ot_cap)
      VSS_Q <- EEV_Q$exp_cost - RP_Q$exp_cost
      
      row_v <- row_v + 1
      results_vss[[row_v]] <- tibble(
        day_index = d, date = planning_date, model = m_print,
        cost_perf_real = cost_perf$total_cost,
        cost_ev_real   = cost_ev_real$total_cost,
        cost_rp_real   = cost_rp_real$total_cost,
        VSS_Q = VSS_Q,
        # Save HOURLY parameters for clearer analysis
        c_short_hourly = c_short_hourly, 
        c_over_hourly = c_over_hourly, 
        ot_cap = ot_cap
      )
    }
  }
  
  vss_df <- bind_rows(results_vss)
  readr::write_csv(vss_df, file.path(base_dir, "vss_summary.csv"))
  return(vss_df)
}

# -----------------
# 4) EXECUTE GRID (Using HOURLY Rates)
# -----------------
grid <- expand.grid(
  c_short_hourly = c(20, 40, 80), # Higher values because they are hourly
  c_over_hourly  = c(0, 0.2, 1),
  ot_cap  = c(1, 2, NA)
)

cat("Starting Optimization Loop for", nrow(grid), "scenarios...\n")

all_summaries <- list()
for (i in 1:nrow(grid)) {
  p <- grid[i, ]
  tag <- paste0("cs", p$c_short_hourly, "_co", p$c_over_hourly, "_ot", ifelse(is.na(p$ot_cap), "NA", p$ot_cap))
  
  all_summaries[[i]] <- run_sensitivity_grid(
    c_short_hourly = p$c_short_hourly, 
    c_over_hourly = p$c_over_hourly, 
    ot_cap = p$ot_cap, 
    tag = tag
  )
}

final_df <- bind_rows(all_summaries)
write_csv(final_df, "outputs_sens/MASTER_sensitivity_all.csv")
cat("ALL DONE.\n")