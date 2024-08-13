library(tidyverse)
library(here)
library(plotly)
library(haven)
library(tools)

# Note: files must have naming convention [SURVEYNAME]-[YEAR]-[QN].sav

# load functions
source(paste0(here(),"/data", "/dashboards", "/functions", "/functions.R"))

# set path for survey files
dir_path <- paste0(here(),"/data_raw/qualtrics/")

# Define the survey name
survey_name <- "PPUS"

ppus_latest <- latest_data(survey_name)

# Variable naming conventions:
# p = past, c = current, n = next, q = quarter

# Fix typo in variable name (fixed in qualtrics, remove this after next data import)
ppus_latest <- ppus_latest %>%
  rename(ip.costs_next_3.q = po.costs_next_3.q)


##########

# These functions each loop over all files in dir_path and output a single file with historical data.
# If methodology changes for calculating indicators in these functions,
# it will change all historical data in the files generated.

agg <- process_agg_data(survey_name, dir_path)
indicators <- process_indicators_data(survey_name, dir_path)
d_indicators <- delta_indicators(survey_name, dir_path)

############# Valuebox values

# get period info
period <- latest_period(survey_name)
curr_period <- paste0(period$year, "-", period$quarter)

# Industry confidence indicator for current period
curr_ic <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(ic) %>% round(2))
curr_ic_dq <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(ic_dq) %>% round(2))
curr_ic_pdq <- sprintf("%.2f", d_indicators %>% filter(Period == curr_period) %>% pull(ic_pdq) %>% round(2))

# Economic uncertainty
curr_iu <- d_indicators %>% filter(Period == curr_period) %>% pull(iu)

# Employment outlook
curr_eo <- d_indicators %>% filter(Period == curr_period) %>% pull(eo)


############## Charts

# Summary

uncertainty_bar <- v_bar_chart(ppus_latest, "po.uncertainty.q")
plans_pie <- pie_chart(ppus_latest, "po.plans.q")
comp_past_3_pie <- pie_chart(ppus_latest, "po.comp_past_3.q")
regions_dist_donut <- donut_chart_cols_pct(ppus_latest, "po.regions_dist.q")

# Production & Orders

orders_past_3_pie <- pie_chart(ppus_latest, "po.orders_past_3.q")
orders_curr_pie <- pie_chart(ppus_latest, "po.orders_curr.q")

stocks_curr_pie <- pie_chart(ppus_latest, "po.stocks_curr.q")

prod_past_3_pie <- pie_chart(ppus_latest, "po.prod_past_3.q")
prod_next_3_pie <- pie_chart(ppus_latest, "po.prod_next_3.q")

prod_lvl_box <- v_box_plot(ppus_latest, "po.prod_lvl.q")
prod_cap_bar <- v_bar_chart(ppus_latest, "po.prod_cap.q")

prod_limits_pie <- pie_chart_cols(ppus_latest, "po.prod_limits.q")

# Inputs & Prices

costs_past_3_pie <- pie_chart(ppus_latest, "ip.costs_past_3.q")
costs_next_3_pie <- pie_chart(ppus_latest, "ip.costs_next_3.q")

prices_past_3_pie <- pie_chart(ppus_latest, "ip.prices_past_3.q")
prices_next_3_pie <- pie_chart(ppus_latest, "ip.prices_next_3.q")

# Labour

emp_past_3_pie <- pie_chart(ppus_latest, "lab.emp_past_3.q")
emp_next_3_pie <- pie_chart(ppus_latest, "lab.emp_next_3.q")

lab_costs_past_3_pie <- pie_chart(ppus_latest, "lab.costs_past_3.q")

lab_costs_lvl_pie <- pie_chart(ppus_latest, "lab.costs_lvl.q")

lab_skill_pie <- pie_chart(ppus_latest, "lab.skill.q")

# Investment & Innovation

inv_past_3_bar <- stacked_v_bar_chart_cols(ppus_latest, "in.inv_past_3.q")
inv_past_3_bar
