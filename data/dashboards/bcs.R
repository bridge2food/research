library(tidyverse)
library(here)
library(plotly)
library(haven)
library(tools)

# load functions
source(paste0(here(),"/data", "/dashboards", "/functions", "/functions.R"))

# set path for bcs files
dir_path <- paste0(here(),"/data_raw/bcs/")

# get data
# note: csv files must be manually saved from the original BCS excel files
bcs_m <- read_csv(paste0(dir_path, "/industry_subsectors_nsa_m_nace2.csv"))
bcs_q <- read_csv(paste0(dir_path, "/industry_subsectors_nsa_q_nace2.csv"))
bcs_q8 <- read_csv(paste0(dir_path, "/industry_subsectors_nsa_q8_nace2.csv"))

# rename first column
bcs_m <- bcs_m %>%
  rename("period" = "Jan-00")
bcs_q <- bcs_q %>%
  rename("period" = "Jan-00")
bcs_q8 <- bcs_q8 %>%
  rename("period" = "Jan-00")

# Remove rows with NA periods
bcs_m <- bcs_m %>% filter(!is.na(period))
bcs_q <- bcs_q %>% filter(!is.na(period))
bcs_q8 <- bcs_q8 %>% filter(!is.na(period))

# Convert date formats for plotting
bcs_m <- bcs_m %>%
  mutate(period = as.Date(sapply(period, convert_mon_yy_to_date)))

bcs_q <- bcs_q %>%
  mutate(period = as.Date(sapply(period, convert_period_to_date)))

bcs_q8 <- bcs_q8 %>%
  mutate(period = as.Date(sapply(period, convert_period_to_date)))

# Manually calculate confidence indicator in order to get two-decimal accuracy
# Filter to 10 most recent years
bcs_m_ea_cof <- bcs_m %>%
  select(period, INDU.EA.10.COF.B.M, INDU.EA.10.2.B.M, INDU.EA.10.4.B.M, INDU.EA.10.5.B.M) %>%
  mutate(cof = (INDU.EA.10.2.B.M - INDU.EA.10.4.B.M + INDU.EA.10.5.B.M)/3) %>%
  filter(period >= as.Date(Sys.Date()) - years(10))
bcs_m_ea_cof$cof <- round(bcs_m_ea_cof$cof, 2)

############# Plots

# EA confidence indicator
bcs_cof <- plot_ly(bcs_m_ea_cof, x = ~period, y = ~cof, type = 'scatter', mode = 'lines')

bcs_cof <- bcs_cof %>%
  layout(title = '',
         xaxis = list(title = 'Period'),
         yaxis = list(title = 'Balance')) %>%
  config(displayModeBar = FALSE)

# Show the plot
bcs_cof

# EA production trend in recent months
bcs_m_ea_prod_trend <- plot_ly(bcs_m, x = ~period, y = ~INDU.EU.10.1.B.M, type = 'scatter', mode = 'lines') %>%
  layout(title = '',
         xaxis = list(title = 'Period'),
         yaxis = list(title = 'Balance')) %>%
  config(displayModeBar = FALSE)

