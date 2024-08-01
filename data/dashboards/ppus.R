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
prev_period <- previous_period(survey_name)
curr_period <- paste0(period$year, "-", period$quarter)


# load data
latest <- latest_data(survey_name)
prev_q <- prev_q_data(survey_name)
prev_y <- prev_y_data(survey_name)
#test <- specified_period_data(survey_name, "2024-Q4")

##############

## arrow direction and color for valuebox
# if (ic > 0) {
#   delta_prod_arrow <- "arrow-up"
#   delta_prod_color <- "success"
# } else {
#   delta_prod_arrow <- "arrow-down"
#   delta_prod_color <- "warning"
# }



##############

# Charts
# q2.2_bar_latest <- v_bar_chart(ppus_latest, "Q2.2", "this is the title")
# q2.2_bar_prev_q <- v_bar_chart(ppus_prev_q, "Q2.2", "this is the title")
# 
# test_chart <- v_bar_chart(ppus_latest, "Q2.2", "Chart title here")
# test_chart
