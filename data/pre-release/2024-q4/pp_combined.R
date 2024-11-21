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

# Replace with the survey names you want to include
survey_names <- c("PPUS", "PMP", "PDP")

# Get the latest period across all surveys
periods <- lapply(survey_names, latest_period)
latest_years <- sapply(periods, function(p) p$year)
latest_quarters <- sapply(periods, function(p) p$quarter)

# Find the maximum year and corresponding maximum quarter
max_year <- max(latest_years)
max_quarter <- max(latest_quarters[latest_years == max_year])
period <- list(year = max_year, quarter = max_quarter)
curr_period <- paste0(period$year, "-", period$quarter)

# Combine data from survey_names and all periods for calculating netbalances and indicators
# Note: bind_rows() removes some variable labels, so we use rbind
pp_all <- lapply(survey_names, function(survey_name) {
  files <- list_survey_files(survey_name)
  data_list <- lapply(files, function(file) {
    time_info <- extract_time_info(file)
    period <- time_info$period
    data <- read_sav(file)
    
    # Apply transforms from transforms.R
    data <- apply_transforms(data, period, survey_name)
    
    return(data)
  })
  # Combine data frames from the current survey
  do.call(rbind, data_list)
})

# Combine data frames from all surveys
pp_all <- do.call(rbind, pp_all)

# Create data frame for only latest period data
pp_latest <- pp_all %>%
  filter(Period == curr_period)

##########

# Calculate netbalances using pp_all
netbalances <- pp_all %>%
  group_by(Period) %>%
  do(calculate_nb(., unique(.$Period))) %>%
  ungroup()

d_netbalances <- calc_deltas(netbalances)

# Calculate indicators using pp_all
indicators <- pp_all %>%
  group_by(Period) %>%
  do(calculate_indicators(., unique(.$Period))) %>%
  ungroup()

d_indicators <- calc_deltas(indicators)





############## Dashboard Content

# Summary

## Industry confidence valuebox
curr_ic <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(ic) %>% round(2))
curr_ic_deltas_html <- create_delta_html(d_indicators, "ic")
curr_ic_tooltip <- "Composite indicator derived from production expectations, current order books, and current stocks of finished products. Higher values for this figure indicate greater industry confidence."

## Business uncertainty valuebox
curr_bu <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(bu) %>% round(2))
curr_bu_deltas_html <- create_delta_html(d_indicators, "bu")
curr_bu_tooltip <- "Net balance of responses to the question asking managers to assess uncertainty in the future development of their business. Higher values for this figuere represent greater uncertainty."

## Employment outlook valuebox
curr_eo <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(eo) %>% round(2))
curr_eo_deltas_html <- create_delta_html(d_indicators, "eo")
curr_eo_tooltip <- "Net balance of responses to the question asking managers about expected changes in staff numbers over the next three months."

## Business Uncertainty
uncertainty_bar <- v_bar_chart(pp_latest, "po.uncertainty.q")
curr_bu_deltas_html_na <- create_delta_html(d_indicators, "bu", show_na = T)
bu_line <- line_plot(indicators, "bu")
bu_text <- "Net balance of responses to the question asking managers to assess uncertainty in the future development of their business. Higher values represent greater uncertainty."

## 3-Year Business Plans
plans_pie <- pie_chart(pp_latest, "po.plans.q")
plans_nb <- get_curr_nb("po.plans.q")
plans_nb_delta_html <- create_delta_html(d_netbalances, "po.plans.q", show_na = T)
plans_line <- line_plot(netbalances, "po.plans.q")
plans_text <- "Net balance of responses to the question asking managers to describe their company's three-year business plans. Higher values indicate greater intent to pursue growth."
plans_nb_html <- nb_html("plans")

## Competitive Position
comp_past_3_pie <- pie_chart(pp_latest, "po.comp_past_3.q")
comp_past_3_nb <- get_curr_nb("po.comp_past_3.q")
comp_past_3_nb_delta_html <- create_delta_html(d_netbalances, "po.comp_past_3.q", show_na=T)
comp_past_3_text <- "Net balance of reponses to question asking managers to describe their company's competitive position over the past three months"
comp_past_3_nb_html <- nb_html("comp_past_3")

## Regional Sales Distribution
regions_dist_donut <- donut_chart_cols_pct(pp_latest, "po.regions_dist.q")

## Segments
segments_pie <- pie_chart_basic(pp_latest, "Segment")

# Production & Orders

## Orders - Past 3 Months
orders_past_3_pie <- pie_chart(pp_latest, "po.orders_past_3.q")
orders_past_3_nb <- get_curr_nb("po.orders_past_3.q")
orders_past_3_nb_delta_html <- create_delta_html(d_netbalances, "po.orders_past_3.q", show_na = T)
orders_past_3_line <- line_plot(netbalances, "po.orders_past_3.q")
orders_past_3_text <- "Net balance of responses to the question asking managers to describe their company's order books over the past three months."
orders_past_3_nb_html <- nb_html("orders_past_3")

## Current Order Books
orders_curr_pie <- pie_chart(pp_latest, "po.orders_curr.q")
orders_curr_nb <- get_curr_nb("po.orders_curr.q")
orders_curr_nb_delta_html <- create_delta_html(d_netbalances, "po.orders_curr.q", show_na = T)
orders_curr_line <- line_plot(netbalances, "po.orders_curr.q")
orders_curr_text <- "Net balance of responses to the question asking managers to describe their company's current order books."
orders_curr_nb_html <- nb_html("orders_curr")

## Production Capacity
prod_cap_pie <- pie_chart(pp_latest, "po.prod_cap.q")
prod_cap_nb <- get_curr_nb("po.prod_cap.q")
prod_cap_nb_delta_html <- create_delta_html(d_netbalances, "po.prod_cap.q", show_na = T)
prod_cap_line <- line_plot(netbalances, "po.prod_cap.q")
prod_cap_text <- "Net balance of responses to the question asking managers to assess their company's current production capacity."
prod_cap_nb_html <- nb_html("prod_cap")

## Current Stocks of Finished Products
stocks_curr_pie <- pie_chart(pp_latest, "po.stocks_curr.q")
stocks_curr_nb <- get_curr_nb("po.stocks_curr.q")
stocks_curr_nb_delta_html <- create_delta_html(d_netbalances, "po.stocks_curr.q", show_na = T)
stocks_curr_line <- line_plot(netbalances, "po.stocks_curr.q")
stocks_curr_text <- "Net balance of responses to the question asking managers to assess their company's current stocks of finished products."
stocks_curr_nb_html <- nb_html("stocks_curr")

## Recent Production
prod_past_3_pie <- pie_chart(pp_latest, "po.prod_past_3.q")
prod_past_3_nb <- get_curr_nb("po.prod_past_3.q")
prod_past_3_nb_delta_html <- create_delta_html(d_netbalances, "po.prod_past_3.q", show_na = T)
prod_past_3_line <- line_plot(netbalances, "po.prod_past_3.q")
prod_past_3_text <- "Net balance of responses to the question asking managers to assess their company's production over the past three months."
prod_past_3_nb_html <- nb_html("prod_past_3")

## Production Expectations
prod_next_3_pie <- pie_chart(pp_latest, "po.prod_next_3.q")
prod_next_3_nb <- get_curr_nb("po.prod_next_3.q")
prod_next_3_nb_delta_html <- create_delta_html(d_netbalances, "po.prod_next_3.q", show_na = T)
prod_next_3_line <- line_plot(netbalances, "po.prod_next_3.q")
prod_next_3_text <- "Net balance of responses to the question asking managers to assess their company's expected production over the next three months."
prod_next_3_nb_html <- nb_html("prod_next_3")

## Production Level
prod_lvl_box <- v_box_plot(pp_latest, "po.prod_lvl.q_1")

## Factors Limiting Production
prod_limits_bar <- v_bar_chart_cols(pp_latest, "po.prod_limits.q")
# prod_limits_pie <- pie_chart_cols(pp_latest, "po.prod_limits.q")


# Inputs & Prices

## Recent Input Costs
costs_past_3_pie <- pie_chart(pp_latest, "ip.costs_past_3.q")
costs_past_3_nb <- get_curr_nb("ip.costs_past_3.q")
costs_past_3_nb_delta_html <- create_delta_html(d_netbalances, "ip.costs_past_3.q", show_na = T)
costs_past_3_line <- line_plot(netbalances, "ip.costs_past_3.q")
costs_past_3_text <- "Net balance of responses to the question asking managers to assess their company's input costs (excluding labour) over the past three months."
costs_past_3_nb_html <- nb_html("costs_past_3")

## Expected Input Costs
costs_next_3_pie <- pie_chart(pp_latest, "ip.costs_next_3.q")
costs_next_3_nb <- get_curr_nb("ip.costs_next_3.q")
costs_next_3_nb_delta_html <- create_delta_html(d_netbalances, "ip.costs_next_3.q", show_na = T)
costs_next_3_line <- line_plot(netbalances, "ip.costs_next_3.q")
costs_next_3_text <- "Net balance of responses to the question asking managers to assess their company's expected input costs (excluding labour) over the next three months."
costs_next_3_nb_html <- nb_html("costs_next_3")

## Recent Selling Prices
prices_past_3_pie <- pie_chart(pp_latest, "ip.prices_past_3.q")
prices_past_3_nb <- get_curr_nb("ip.prices_past_3.q")
prices_past_3_nb_delta_html <- create_delta_html(d_netbalances, "ip.prices_past_3.q", show_na = T)
prices_past_3_line <- line_plot(netbalances, "ip.prices_past_3.q")
prices_past_3_text <- "Net balance of responses to the question asking managers to assess their company's selling prices over the past three months."
prices_past_3_nb_html <- nb_html("prices_past_3")

## Expected Selling Prices
prices_next_3_pie <- pie_chart(pp_latest, "ip.prices_next_3.q")
prices_next_3_nb <- get_curr_nb("ip.prices_next_3.q")
prices_next_3_nb_delta_html <- create_delta_html(d_netbalances, "ip.prices_next_3.q", show_na = T)
prices_next_3_line <- line_plot(netbalances, "ip.prices_next_3.q")
prices_next_3_text <- "Net balance of responses to the question asking managers to assess their company's expected selling prices over the next three months."
prices_next_3_nb_html <- nb_html("prices_next_3")

# Labour

## Recent Staff Numbers
emp_past_3_pie <- pie_chart(pp_latest, "lab.emp_past_3.q")
emp_past_3_nb <- get_curr_nb("lab.emp_past_3.q")
emp_past_3_nb_delta_html <- create_delta_html(d_netbalances, "lab.emp_past_3.q", show_na = T)
emp_past_3_line <- line_plot(netbalances, "lab.emp_past_3.q")
emp_past_3_text <- "Net balance of responses to the question asking managers to assess their company's staff numbers over the past three months. Higher numbers indicate greater recent hiring."
emp_past_3_nb_html <- nb_html("emp_past_3")

## Expected Staff Numbers
emp_next_3_pie <- pie_chart(pp_latest, "lab.emp_next_3.q")
emp_next_3_nb <- get_curr_nb("lab.emp_next_3.q")
emp_next_3_nb_delta_html <- create_delta_html(d_netbalances, "lab.emp_next_3.q", show_na = T)
emp_next_3_line <- line_plot(netbalances, "lab.emp_next_3.q")
emp_next_3_text <- "Net balance of responses to the question asking managers to assess their company's expected staff numbers over the next three months. Higher numbers indicate greater expected hiring."
emp_next_3_nb_html <- nb_html("emp_next_3")

## Recent Labour Costs
lab_costs_past_3_pie <- pie_chart(pp_latest, "lab.costs_past_3.q")
lab_costs_past_3_nb <- get_curr_nb("lab.costs_past_3.q")
lab_costs_past_3_nb_delta_html <- create_delta_html(d_netbalances, "lab.costs_past_3.q", show_na = T)
lab_costs_past_3_line <- line_plot(netbalances, "lab.costs_past_3.q")
lab_costs_past_3_text <- "Net balance of responses to the question asking managers to assess their company's personnel costs over the past three months."
lab_costs_past_3_nb_html <- nb_html("lab_costs_past_3")

## Labor Costs Level
lab_costs_lvl_pie <- pie_chart(pp_latest, "lab.costs_lvl.q")
lab_costs_lvl_nb <- get_curr_nb("lab.costs_lvl.q")
lab_costs_lvl_nb_delta_html <- create_delta_html(d_netbalances, "lab.costs_lvl.q", show_na = T)
lab_costs_lvl_line <- line_plot(netbalances, "lab.costs_lvl.q")
lab_costs_lvl_text <- "Net balance of responses to the question asking managers to assess current labour costs in their sector."
lab_costs_lvl_nb_html <- nb_html("lab_costs_lvl")

## Finding Skilled Labour
lab_skill_pie <- pie_chart(pp_latest, "lab.skill.q")
lab_skill_nb <- get_curr_nb("lab.skill.q")
lab_skill_nb_delta_html <- create_delta_html(d_netbalances, "lab.skill.q", show_na = T)
lab_skill_line <- line_plot(netbalances, "lab.skill.q")
lab_skill_text <- "Net balance of responses to the question asking managers to assess the difficulty of finding sufficiently skilled labour."
lab_skill_nb_html <- nb_html("lab_skill")

# Investment & Innovation

## Recent Investments
inv_past_3_bar <- stacked_v_bar_chart_cols(pp_latest, "in.inv_past_3.q")
### Total
inv_past_3_tot_nb <- get_curr_nb("in.inv_past_3.q_1")
inv_past_3_tot_nb_delta_html <- create_delta_html(d_netbalances, "in.inv_past_3.q_1", show_na = T)
### Machinery & Equipment
inv_past_3_me_nb <- get_curr_nb("in.inv_past_3.q_2")
inv_past_3_me_nb_delta_html <- create_delta_html(d_netbalances, "in.inv_past_3.q_2", show_na = T)
### Land, Building, & Infrastructure
inv_past_3_lbi_nb <- get_curr_nb("in.inv_past_3.q_3")
inv_past_3_lbi_nb_delta_html <- create_delta_html(d_netbalances, "in.inv_past_3.q_3", show_na = T)
### Nonphysical Capital
inv_past_3_nc_nb <- get_curr_nb("in.inv_past_3.q_4")
inv_past_3_nc_nb_delta_html <- create_delta_html(d_netbalances, "in.inv_past_3.q_4", show_na = T)

inv_past_3_text <- "Net balances of responses to the questions asking managers to describe their total investments and the structure of investments over the past three months."

inv_past_3_nb_html <- paste0(
  '<div class="d-flex flex-row flex-wrap justify-content-around mt-5 text-center">\n',
  
  '<div class="p-2">\n',
  '<h4>Total</h4>\n',
  '<span class="display-1">', inv_past_3_tot_nb, '</span><br>\n',
  inv_past_3_tot_nb_delta_html, '\n', 
  '</div>\n',
  
  '<div class="p-2">\n',
  '<h4>Machinery & Equipment</h4>\n',
  '<span class="display-1">', inv_past_3_me_nb, '</span><br>\n',
  inv_past_3_me_nb_delta_html, '\n', 
  '</div>\n',
  
  '<div class="p-2">\n',
  '<h4>Land, Building, & Infrastructure</h4>\n',
  '<span class="display-1">', inv_past_3_lbi_nb, '</span><br>\n',
  inv_past_3_lbi_nb_delta_html, '\n', 
  '</div>\n',
  
  '<div class="p-2">\n',
  '<h4>Nonphysical Capital</h4>\n',
  '<span class="display-1">', inv_past_3_nc_nb, '</span><br>\n',
  inv_past_3_nc_nb_delta_html, '\n', 
  '</div>\n',
  
  '</div>\n',  # End of the row
  
  # Additional text section
  '<div class="text-muted small m-4 p-4 text-center">\n',
  inv_past_3_text, '\n',
  '</div>'
)

## Current Year Investments
inv_curr_y_pie <- pie_chart(pp_latest, "in.inv_curr_y.q4")
inv_curr_y_nb <- get_curr_nb("in.inv_curr_y.q4")
inv_curr_y_nb_delta_html <- create_delta_html(d_netbalances, "in.inv_curr_y.q4", show_na = T)
inv_curr_y_line <- line_plot(netbalances, "in.inv_curr_y.q4")
inv_curr_y_text <- "Net balance of responses to the question asking managers to describe the current year's investments compared to last year's. Only asked in Q4."
inv_curr_y_nb_html <- nb_html("inv_curr_y")

## Next Year Investment Expectations
inv_next_y_pie <- pie_chart(pp_latest, "in.inv_next_y.q4")
inv_next_y_nb <- get_curr_nb("in.inv_next_y.q4")
inv_next_y_nb_delta_html <- create_delta_html(d_netbalances, "in.inv_next_y.q4", show_na = T)
inv_next_y_line <- line_plot(netbalances, "in.inv_next_y.q4")
inv_next_y_text <- "Net balance of responses to the question asking managers to describe their expectations for next year's investments compared to this year's. Only asked in Q4."
inv_next_y_nb_html <- nb_html("inv_next_y")

## Structure of Investments
inv_structure <- stacked_v_bar_chart_cols_nk(pp_latest, "in.inv_structure.q4")

## Drivers of Investments
inv_drivers <- stacked_v_bar_chart_cols_nk(pp_latest, "in.inv_drivers.q4")

# Notes

pre_release_period <- "December 2-15 2024"
