library(tidyverse)
library(here)
library(plotly)
library(haven)
library(tools)

# Note: files must have naming convention [SURVEYNAME]-[YEAR]-[QN].sav

# load functions
source(paste0(here(),"/data/", "dashboards/", "functions.R"))

# set path for survey files
dir_path <- paste0(here(),"/data_raw/qualtrics/")

# Define the survey name
survey_name <- "PPUS"

##########

# These functions each loop over all files in dir_path and output a single file with historical data.
# If methodology changes for calculating indicators in these functions,
# it will change all historical data in the files generated.

agg <- process_agg_data(survey_name, dir_path)
indicators <- process_indicators_data(survey_name, dir_path)
d_indicators <- delta_indicators(survey_name, dir_path)

##########

# get period info
period <- latest_period(survey_name)
curr_period <- paste0(period$year, "-", period$quarter)

# Get the value of ic for the current period
curr_ic <- d_indicators %>% filter(Period == curr_period) %>% pull(ic)
curr_ic_dq <- d_indicators %>% filter(Period == curr_period) %>% pull(ic_dq)
curr_ic_pdq <- d_indicators %>% filter(Period == curr_period) %>% pull(ic_pdq)

##############

## arrows and colors for valueboxs
if (curr_ic > 0) {
  curr_ic_icon <- "arrow-up"
  curr_ic_color <- "success"
} else {
  curr_ic_icon <- "arrow-down"
  curr_ic_color <- "warning"
}

if (curr_ic_dq > 0) {
  curr_ic_dq_icon <- "caret-up-square"
  curr_ic_dq_color <- "success"
} else {
  curr_ic_dq_icon <- "caret-down-square"
  curr_ic_dq_color <- "warning"
}

if (curr_ic_pdq > 0) {
  curr_ic_pdq_icon <- "arrow-up"
  curr_ic_pdq_color <- "success"
} else {
  curr_ic_pdq_icon <- "arrow-down"
  curr_ic_pdq_color <- "warning"
}



##############

# Charts
# q2.2_bar_latest <- v_bar_chart(ppus_latest, "Q2.2", "this is the title")
# q2.2_bar_prev_q <- v_bar_chart(ppus_prev_q, "Q2.2", "this is the title")
# 
# test_chart <- v_bar_chart(ppus_latest, "Q2.2", "Chart title here")
# test_chart
