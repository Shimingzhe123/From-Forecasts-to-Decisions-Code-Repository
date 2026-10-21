# ==============================================================================
# FULL VISUALIZATION SCRIPT FOR JOM MANUSCRIPT (UPDATED)
# Clean version: EV vs RP only
# - Remove all "EV + Safety Buffer" content
# - Keep EV, RP, VSS, EVPI
# - EEV is treated as a benchmark, not as a plotted policy
# - Added diag_summary.csv integration for CRPS and other diagnostics
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggrepel)
  library(readr)
  library(grid)
})

# ==============================================================================
# 0. USER SETTINGS
# ==============================================================================
input_file <- "01data/optimization_results_main.csv"
diag_file  <- "outputs/diag_summary.csv"  # <--- 新增：包含 CRPS 的诊断数据
output_dir <- "plots_jom_clean"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. HELPER FUNCTIONS
# ==============================================================================

# Pick the first existing column from a list of candidates
pick_col <- function(df, candidates, required = TRUE, label = NULL) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) return(hit[1])
  
  if (required) {
    stop(
      sprintf(
        "Required column not found%s. Tried: %s\nAvailable columns are:\n%s",
        if (!is.null(label)) paste0(" for ", label) else "",
        paste(candidates, collapse = ", "),
        paste(names(df), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  return(NA_character_)
}

# Safe mean
safe_mean <- function(x) mean(x, na.rm = TRUE)

# Pretty message
msg <- function(...) cat(paste0(..., "\n"))

# ==============================================================================
# 2. JOM THEME & COLOR PALETTE
# ==============================================================================
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

# ==============================================================================
# 3. LOAD DATA
# ==============================================================================
if (!file.exists(input_file)) {
  stop(sprintf("Input file not found: %s", input_file), call. = FALSE)
}

df <- read_csv(input_file, show_col_types = FALSE)
msg("Loaded main data from: ", input_file)
msg("Rows: ", nrow(df), " | Columns: ", ncol(df))

# Load diagnostic data (CRPS) if available
if (file.exists(diag_file)) {
  diag_df <- read_csv(diag_file, show_col_types = FALSE) %>%
    mutate(date = as.Date(date))
  msg("Loaded diagnostic data from: ", diag_file)
  msg("Rows: ", nrow(diag_df), " | Columns: ", ncol(diag_df))
} else {
  diag_df <- NULL
  msg("Diagnostic data not found, skipping merge: ", diag_file)
}

# ==============================================================================
# 4. COLUMN RESOLUTION & MERGE
# ==============================================================================

# Core columns from main dataset
model_col <- pick_col(df, c("model", "Model"), label = "model")
date_col  <- pick_col(df, c("date", "day", "eval_date", "target_date"), required = FALSE, label = "date")

# Rename core columns to establish join keys
df_plot <- df %>% rename(model = !!model_col)

if (!is.na(date_col)) {
  df_plot <- df_plot %>% 
    rename(eval_id = !!date_col) %>%
    mutate(eval_id = as.Date(eval_id))
} else {
  df_plot$eval_id <- seq_len(nrow(df_plot))
}

# MERGE diagnostic data (brings in CRPS_day, EV_under_hours, etc.)
if (!is.null(diag_df)) {
  df_plot <- df_plot %>%
    left_join(diag_df, by = c("model" = "model", "eval_id" = "date"))
}

# Now map the metrics from the combined dataset
crps_col <- pick_col(
  df_plot,
  c("CRPS_day", "daily_crps", "crps_day", "crps", "mean_crps"),
  required = FALSE,
  label = "CRPS"
)

# Added EV_under_hours / RP_under_hours from diag_df to the candidate lists
ev_short_col <- pick_col(
  df_plot,
  c("EV_under_hours", "short_hours_ev", "EV_short_hours", "understaff_hours_ev", "ev_understaff_hours", "cost_short_hours_ev", "shortage_hours_ev"),
  required = FALSE,
  label = "EV understaffing"
)

rp_short_col <- pick_col(
  df_plot,
  c("RP_under_hours", "short_hours_rp", "RP_short_hours", "understaff_hours_rp", "rp_understaff_hours", "cost_short_hours_rp", "shortage_hours_rp"),
  required = FALSE,
  label = "RP understaffing"
)

ev_sl_col <- pick_col(df_plot, c("service_level_ev", "EV_service_level", "mean_SL_EV", "sl_ev"), required = FALSE, label = "EV service level")
rp_sl_col <- pick_col(df_plot, c("service_level_rp", "RP_service_level", "mean_SL_RP", "sl_rp"), required = FALSE, label = "RP service level")
ev_cost_col <- pick_col(df_plot, c("cost_real_ev", "EV_cost", "total_cost_ev", "realized_cost_ev", "eval_cost_ev"), required = FALSE, label = "EV cost")
rp_cost_col <- pick_col(df_plot, c("cost_real_rp", "RP_cost", "total_cost_rp", "realized_cost_rp", "eval_cost_rp"), required = FALSE, label = "RP cost")
vss_col <- pick_col(df_plot, c("VSS", "vss"), required = FALSE, label = "VSS")
evpi_col <- pick_col(df_plot, c("EVPI", "evpi"), required = FALSE, label = "EVPI")
eev_col <- pick_col(df_plot, c("EEV", "eev", "EEV_Q", "eev_total_cost", "eval_cost_eev"), required = FALSE, label = "EEV")

# Safely rename mapped columns to standardized names for plotting
if (!is.na(crps_col) && crps_col != "CRPS_day")             names(df_plot)[names(df_plot) == crps_col] <- "CRPS_day"
if (!is.na(ev_short_col) && ev_short_col != "EV_short_hours") names(df_plot)[names(df_plot) == ev_short_col] <- "EV_short_hours"
if (!is.na(rp_short_col) && rp_short_col != "RP_short_hours") names(df_plot)[names(df_plot) == rp_short_col] <- "RP_short_hours"
if (!is.na(ev_sl_col) && ev_sl_col != "EV_service_level")     names(df_plot)[names(df_plot) == ev_sl_col] <- "EV_service_level"
if (!is.na(rp_sl_col) && rp_sl_col != "RP_service_level")     names(df_plot)[names(df_plot) == rp_sl_col] <- "RP_service_level"
if (!is.na(ev_cost_col) && ev_cost_col != "EV_cost")          names(df_plot)[names(df_plot) == ev_cost_col] <- "EV_cost"
if (!is.na(rp_cost_col) && rp_cost_col != "RP_cost")          names(df_plot)[names(df_plot) == rp_cost_col] <- "RP_cost"
if (!is.na(vss_col) && vss_col != "VSS")                      names(df_plot)[names(df_plot) == vss_col] <- "VSS"
if (!is.na(evpi_col) && evpi_col != "EVPI")                   names(df_plot)[names(df_plot) == evpi_col] <- "EVPI"
if (!is.na(eev_col) && eev_col != "EEV")                      names(df_plot)[names(df_plot) == eev_col] <- "EEV"

# Model ordering:
# Prefer CRPS if available, otherwise VSS, otherwise alphabetical
if ("CRPS_day" %in% names(df_plot)) {
  model_order <- df_plot %>%
    group_by(model) %>%
    summarise(order_metric = median(CRPS_day, na.rm = TRUE), .groups = "drop") %>%
    arrange(order_metric) %>%
    pull(model)
} else if ("VSS" %in% names(df_plot)) {
  model_order <- df_plot %>%
    group_by(model) %>%
    summarise(order_metric = mean(VSS, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(order_metric)) %>%
    pull(model)
} else {
  model_order <- sort(unique(df_plot$model))
}

df_plot$model <- factor(df_plot$model, levels = model_order)

# ==============================================================================
# 5. FIGURE 1: CRPS
# ==============================================================================
if ("CRPS_day" %in% names(df_plot)) {
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
  
  ggsave(file.path(output_dir, "Fig1_CRPS.pdf"), plot = p1, width = 7.5, height = 4.5)
  msg("Saved: Fig1_CRPS.pdf")
} else {
  msg("Skipped Figure 1: CRPS column not found.")
}

# ==============================================================================
# 6. FIGURE 2: UNDERSTAFFING EV vs RP
# ==============================================================================
if (all(c("EV_short_hours", "RP_short_hours") %in% names(df_plot))) {
  df_shortage <- df_plot %>%
    select(eval_id, model, EV_short_hours, RP_short_hours) %>%
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
    geom_boxplot(
      alpha = 0.85,
      outlier.size = 0.5,
      outlier.alpha = 0.3,
      width = 0.7,
      position = position_dodge(0.75)
    ) +
    scale_fill_manual(values = policy_colors) +
    labs(
      title = "Understaffing Risk under EV and Stochastic RP",
      subtitle = "Comparing point-based planning with scenario-based stochastic optimization.",
      x = "Forecasting Model",
      y = "Realized Daily Understaffing (Hours)"
    ) +
    jom_theme
  
  ggsave(file.path(output_dir, "Fig2_Understaffing_EV_vs_RP.pdf"), plot = p2, width = 8.5, height = 5)
  msg("Saved: Fig2_Understaffing_EV_vs_RP.pdf")
} else {
  msg("Skipped Figure 2: understaffing columns not found.")
}

# ==============================================================================
# 7. FIGURE 3: SERVICE LEVEL EV vs RP
# ==============================================================================
if (all(c("EV_service_level", "RP_service_level") %in% names(df_plot))) {
  df_service <- df_plot %>%
    select(eval_id, model, EV_service_level, RP_service_level) %>%
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
  
  p3 <- ggplot(df_service, aes(x = model, y = Mean_SL, fill = Policy)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "black",
      linewidth = 0.3
    ) +
    scale_fill_manual(values = policy_colors) +
    coord_cartesian(ylim = c(0.70, 1.0)) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    annotate("text", x = 0.72, y = 0.96, label = "Target 95%", color = "gray40", fontface = "bold", size = 3.5) +
    labs(
      #title = "Average Service Level by Policy",
      #subtitle = "Stochastic RP improves service reliability relative to point-based EV.",
      x = "Forecasting Model",
      y = "Average Service Level"
    ) +
    jom_theme
  
  ggsave(file.path(output_dir, "Fig3_ServiceLevel_EV_vs_RP.pdf"), plot = p3, width = 8.5, height = 5)
  msg("Saved: Fig3_ServiceLevel_EV_vs_RP.pdf")
} else {
  msg("Skipped Figure 3: service level columns not found.")
}

# ==============================================================================
# 8. FIGURE 4: COST DISTRIBUTION EV vs RP
# ==============================================================================
if (all(c("EV_cost", "RP_cost") %in% names(df_plot))) {
  df_cost <- df_plot %>%
    select(eval_id, model, EV_cost, RP_cost) %>%
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
    geom_boxplot(
      alpha = 0.85,
      outlier.shape = NA,
      width = 0.7,
      position = position_dodge(0.75)
    ) +
    coord_cartesian(ylim = quantile(df_cost$Cost, c(0.01, 0.95), na.rm = TRUE)) +
    scale_fill_manual(values = policy_colors) +
    labs(
      title = "Distribution of Realized Daily Total Costs",
      subtitle = "Scenario-based RP reduces tail-cost exposure relative to point-based EV.",
      x = "Forecasting Model",
      y = "Daily Operational Cost"
    ) +
    jom_theme
  
  ggsave(file.path(output_dir, "Fig4_TotalCost_EV_vs_RP.pdf"), plot = p4, width = 8.5, height = 5)
  msg("Saved: Fig4_TotalCost_EV_vs_RP.pdf")
} else {
  msg("Skipped Figure 4: cost columns not found.")
}

# ==============================================================================
# 9. FIGURE 5: VSS vs EVPI
# ==============================================================================
if (all(c("VSS", "EVPI") %in% names(df_plot))) {
  df_value <- df_plot %>%
    group_by(model) %>%
    summarise(
      mean_VSS = safe_mean(VSS),
      mean_EVPI = safe_mean(EVPI),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(mean_VSS, mean_EVPI),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    mutate(
      Metric = recode(
        Metric,
        mean_VSS = "VSS (Optimization Value)",
        mean_EVPI = "EVPI (Prediction Potential)"
      ),
      Metric = factor(Metric, levels = names(value_colors))
    )
  
  p5 <- ggplot(df_value, aes(x = model, y = Value, fill = Metric)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "black",
      linewidth = 0.3
    ) +
    scale_fill_manual(values = value_colors) +
    labs(
      title = "Sources of Value: Optimization versus Prediction",
      subtitle = "VSS captures the value of stochastic optimization; EVPI captures the remaining value of perfect information.",
      x = "Forecasting Model",
      y = "Average Daily Value"
    ) +
    jom_theme
  
  ggsave(file.path(output_dir, "Fig5_VSS_vs_EVPI.pdf"), plot = p5, width = 8.5, height = 5)
  msg("Saved: Fig5_VSS_vs_EVPI.pdf")
} else {
  msg("Skipped Figure 5: VSS and/or EVPI columns not found.")
}

# ==============================================================================
# 10. FIGURE 6: MODEL LANDSCAPE
# ==============================================================================
if (all(c("VSS", "EVPI") %in% names(df_plot))) {
  df_landscape <- df_plot %>%
    group_by(model) %>%
    summarise(
      mean_VSS = safe_mean(VSS),
      mean_EVPI = safe_mean(EVPI),
      .groups = "drop"
    )
  
  p6 <- ggplot(df_landscape, aes(x = mean_VSS, y = mean_EVPI, label = model)) +
    geom_point(size = 5, color = "#2B2D42", alpha = 0.85) +
    geom_text_repel(point.padding = 0.8, size = 4.5, fontface = "bold") +
    labs(
      title = "Decision Landscape: Optimization Value versus Information Value",
      subtitle = "Lower-left indicates both limited residual uncertainty and limited incremental value from additional hedging.",
      x = "Average VSS (Value of Stochastic Solution)",
      y = "Average EVPI (Expected Value of Perfect Information)"
    ) +
    jom_theme +
    theme(panel.grid.major.x = element_line(color = "gray85", linetype = "dashed", linewidth = 0.4))
  
  ggsave(file.path(output_dir, "Fig6_Model_Landscape.pdf"), plot = p6, width = 6.5, height = 5.5)
  msg("Saved: Fig6_Model_Landscape.pdf")
} else {
  msg("Skipped Figure 6: VSS and/or EVPI columns not found.")
}

# ==============================================================================
# 11. OPTIONAL: EEV SUMMARY TABLE (NOT A POLICY FIGURE)
# ==============================================================================
if ("EEV" %in% names(df_plot)) {
  eev_summary <- df_plot %>%
    group_by(model) %>%
    summarise(
      mean_EEV = safe_mean(EEV),
      mean_VSS = if ("VSS" %in% names(df_plot)) safe_mean(VSS) else NA_real_,
      mean_EVPI = if ("EVPI" %in% names(df_plot)) safe_mean(EVPI) else NA_real_,
      .groups = "drop"
    ) %>%
    arrange(desc(mean_VSS))
  
  write_csv(eev_summary, file.path(output_dir, "Table_EEV_Summary.csv"))
  msg("Saved: Table_EEV_Summary.csv")
  msg("Note: EEV detected and exported as a benchmark table, not plotted as a third policy.")
} else {
  msg("No EEV column found. Skipped EEV summary export.")
}

msg("")
msg("Clean JOM visualizations generated successfully in: ", output_dir)
msg("This version integrates diag_summary.csv for CRPS, removes Buffer policy references, and keeps EV vs RP only.")