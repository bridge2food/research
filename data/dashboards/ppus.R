library(tidyverse)
library(here)
library(plotly)
library(haven)
library(tools)

# Note: files must have naming convention [SURVEYNAME]-[YEAR]-[QN].sav

# load functions
source(paste0(here(),"/data/", "dashboards/", "functions.R"))

# set path for survey files
file_path <- paste0(here(),"/data_raw/qualtrics/")

# define time variables
latest_quarter <- latest_q("PPUS", file_path)
latest_year <- latest_y("PPUS", file_path)
previous_quarter <- previous_q("PPUS", file_path) #returns a list
previous_year <- previous_y("PPUS", file_path)
latest_period <- paste(latest_quarter, latest_year)

# load data
ppus_latest <- latest_data("PPUS", file_path)
ppus_prev_q <- prev_q_data("PPUS", file_path)
#ppus_prev_y <- prev_y_data("PPUS", file_path)
#ppus_test <- get_data("PPUS", file_path, "Q3", 2024)

# Filter for numeric columns
numeric_cols <- sapply(ppus_latest, is.numeric)
ppus_numeric <- ppus_latest[, numeric_cols]
# Aggregate all numeric columns
means <- colMeans(ppus_numeric, na.rm = TRUE)
agg <- as.data.frame(t(means), stringsAsFactors = FALSE)
agg$Period <- paste0(latest_year, "-", latest_quarter)
agg <- agg[, c("Period", names(means))] # Ensure Period is the first column

# append agg data to time series file
ppus_ts <- read_rds(paste0(file_path, "/ppus-ts.rds")) %>%
  bind_rows(agg) %>%
  distinct() %>%
  arrange(Period) %>%
  write_rds(file=paste0(file_path, "/ppus-ts.rds"))


# Industry confidence
ic <- mean(ppus_latest$Q2.3, na.rm = T) + mean(ppus_latest$Q2.4, na.rm = T) + mean(ppus_latest$Q2.5, na.rm = T)
ic <- round(ic, 2)
ic1 <- agg$Q2.3 + agg$Q2.4 + agg$Q2.5

## arrow direction and color for valuebox
if (ic > 0) {
  delta_prod_arrow <- "arrow-up"
  delta_prod_color <- "success"
} else {
  delta_prod_arrow <- "arrow-down"
  delta_prod_color <- "warning"
}

# Charts
q2.2_bar_latest <- v_bar_chart(ppus_latest, "Q2.2", "this is the title")
q2.2_bar_prev_q <- v_bar_chart(ppus_prev_q, "Q2.2", "this is the title")

test_chart <- v_bar_chart(ppus_latest, "Q2.2", "Chart title here")
test_chart


