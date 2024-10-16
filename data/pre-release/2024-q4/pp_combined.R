library(tidyverse)
library(here)
library(plotly)
library(haven)
library(tools)

# Note: files must have naming convention [SURVEYNAME]-[YEAR]-[QN].sav

# load functions
source(paste0(here(),"/data", "/dashboards", "/functions", "/functions.R"))

# Toggle to include pre-release data
include_pre_release <- T  # Set to TRUE to include pre-release data

# Set path for survey files
base_dir <- paste0(here(), "/data_raw/qualtrics/")

if (include_pre_release) {
  dir_path <- c(paste0(base_dir, "pre-release/"), base_dir)
} else {
  dir_path <- base_dir
}

# Define the survey names
survey_names <- c("PPUS", "PMP", "PDP")  # Replace with the survey names you want to include

# Combine data from survey_names for latest period
pp_latest <- lapply(survey_names, function(survey_name) {
  latest_data(survey_name) %>%
    filter(Finished == 1) # %>%
    # select(po.prod_past_3.q:last_col())
}) %>% do.call(rbind, .)

# Combine data from survey_names and all periods for calculating agg and indicators # Note: bind_rows() removes some variable labels
pp_all <- lapply(survey_names, function(survey_name) {
  files <- list_survey_files(survey_name)
  data_list <- lapply(files, function(file) {
    time_info <- extract_time_info(file)
    period <- time_info$period
    data <- read_sav(file) %>%
      filter(Finished == 1) %>%
      mutate(Period = period)
    return(data)
  })
  # Combine data frames from the current survey
  do.call(rbind, data_list)
})

# Combine data frames from all surveys
pp_all <- do.call(rbind, pp_all)


# Variable naming conventions:
# p = past, c = current, n = next, q = quarter

##########

# These functions each loop over all files in dir_path and output a single file with historical data.
# If methodology changes for calculating indicators in these functions,
# it will change all historical data in the files generated.

# Calculate aggregated data using pp_all
agg <- pp_all %>%
  group_by(Period) %>%
  do(calculate_agg(., unique(.$Period))) %>%
  ungroup()


# Calculate indicators using pp_all
indicators <- pp_all %>%
  group_by(Period) %>%
  do(calculate_indicators(., unique(.$Period))) %>%
  ungroup()

d_indicators <- delta_indicators(indicators)

############# Valuebox values

# Get the latest period across all surveys
periods <- lapply(survey_names, latest_period)
latest_years <- sapply(periods, function(p) p$year)
latest_quarters <- sapply(periods, function(p) p$quarter)

# Find the maximum year and corresponding maximum quarter
max_year <- max(latest_years)
max_quarter <- max(latest_quarters[latest_years == max_year])

period <- list(year = max_year, quarter = max_quarter)
curr_period <- paste0(period$year, "-", period$quarter)

# Industry confidence indicator for current period (dq = delta quarter, pdq = percent delta quarter)
curr_ic <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(ic) %>% round(2))
curr_ic_dq <- d_indicators %>% filter(Period == curr_period) %>% pull(ic_dq) %>% round(2)
curr_ic_pdq <- d_indicators %>% filter(Period == curr_period) %>% pull(ic_pdq) %>% round(2)

# Business uncertainty
curr_bu <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(bu) %>% round(2))
curr_bu_dq <- d_indicators %>% filter(Period == curr_period) %>% pull(bu_dq) %>% round(2)
curr_bu_pdq <- d_indicators %>% filter(Period == curr_period) %>% pull(bu_pdq) %>% round(2)

# Employment outlook
curr_eo <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(eo) %>% round(2))
curr_eo_dq <- d_indicators %>% filter(Period == curr_period) %>% pull(eo_dq) %>% round(2)
curr_eo_pdq <- d_indicators %>% filter(Period == curr_period) %>% pull(eo_pdq) %>% round(2)

# Create HTML snippets for each delta indicator
curr_ic_dq_html <- create_delta_html(curr_ic_dq)
curr_ic_pdq_html <- create_delta_html(curr_ic_pdq, is_percent = TRUE)

curr_bu_dq_html <- create_delta_html(curr_bu_dq)
curr_bu_pdq_html <- create_delta_html(curr_bu_pdq, is_percent = TRUE)

curr_eo_dq_html <- create_delta_html(curr_eo_dq)
curr_eo_pdq_html <- create_delta_html(curr_eo_pdq, is_percent = TRUE)

# Combine the delta indicators with spacing only if both are present
# Industry Confidence
if (curr_ic_dq_html != "" && curr_ic_pdq_html != "") {
  curr_ic_deltas_html <- paste0(curr_ic_dq_html, " &nbsp;&nbsp; ", curr_ic_pdq_html)
} else {
  # If either delta indicator is missing, do not display any deltas
  curr_ic_deltas_html <- ""
}
# Business Uncertainty
if (curr_bu_dq_html != "" && curr_bu_pdq_html != "") {
  curr_bu_deltas_html <- paste0(curr_bu_dq_html, " &nbsp;&nbsp; ", curr_bu_pdq_html)
} else {
  # If either delta indicator is missing, do not display any deltas
  curr_bu_deltas_html <- ""
}
# Employment Outlook
if (curr_eo_dq_html != "" && curr_eo_pdq_html != "") {
  curr_eo_deltas_html <- paste0(curr_eo_dq_html, " &nbsp;&nbsp; ", curr_eo_pdq_html)
} else {
  # If either delta indicator is missing, do not display any deltas
  curr_eo_deltas_html <- ""
}

# Define tooltip text for Industry Confidence
curr_ic_tooltip <- "Composite indicator derived from production expectations, current order books, and current stocks of finished products. Higher values for this figure indicate greater industry confidence."
curr_bu_tooltip <- "Net balance of responses to the question asking managers to assess uncertainty in the future development of their business. Higher values for this figuere represent greater uncertainty."
curr_eo_tooltip <- "Net balance of responses to the question asking managers about expected changes in staff numbers over the next three months."


############## Charts

# Summary

uncertainty_bar <- v_bar_chart(pp_latest, "po.uncertainty.q")
plans_pie <- pie_chart(pp_latest, "po.plans.q")
comp_past_3_pie <- pie_chart(pp_latest, "po.comp_past_3.q")
regions_dist_donut <- donut_chart_cols_pct(pp_latest, "po.regions_dist.q")

# Production & Orders

orders_past_3_pie <- pie_chart(pp_latest, "po.orders_past_3.q")
orders_curr_pie <- pie_chart(pp_latest, "po.orders_curr.q")

stocks_curr_pie <- pie_chart(pp_latest, "po.stocks_curr.q")

prod_past_3_pie <- pie_chart(pp_latest, "po.prod_past_3.q")
prod_next_3_pie <- pie_chart(pp_latest, "po.prod_next_3.q")

prod_lvl_box <- v_box_plot(pp_latest, "po.prod_lvl.q_1")
prod_cap_bar <- v_bar_chart(pp_latest, "po.prod_cap.q")

prod_limits_pie <- pie_chart_cols(pp_latest, "po.prod_limits.q")
prod_limits_bar <- v_bar_chart_cols(pp_latest, "po.prod_limits.q")

# Inputs & Prices

costs_past_3_pie <- pie_chart(pp_latest, "ip.costs_past_3.q")
costs_next_3_pie <- pie_chart(pp_latest, "ip.costs_next_3.q")

prices_past_3_pie <- pie_chart(pp_latest, "ip.prices_past_3.q")
prices_next_3_pie <- pie_chart(pp_latest, "ip.prices_next_3.q")

# Labour

emp_past_3_pie <- pie_chart(pp_latest, "lab.emp_past_3.q")
emp_next_3_pie <- pie_chart(pp_latest, "lab.emp_next_3.q")

lab_costs_past_3_pie <- pie_chart(pp_latest, "lab.costs_past_3.q")

lab_costs_lvl_pie <- pie_chart(pp_latest, "lab.costs_lvl.q")

lab_skill_pie <- pie_chart(pp_latest, "lab.skill.q")

# Investment & Innovation

inv_past_3_bar <- stacked_v_bar_chart_cols(pp_latest, "in.inv_past_3.q")

inv_curr_y_pie <- pie_chart(pp_latest, "in.inv_curr_y.q4")
inv_next_y_pie <- pie_chart(pp_latest, "in.inv_next_y.q4")

inv_structure <- stacked_v_bar_chart_cols_nk(pp_latest, "in.inv_structure.q4")
inv_drivers <- stacked_v_bar_chart_cols_nk(pp_latest, "in.inv_drivers.q4")

