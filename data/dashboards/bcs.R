library(tidyverse)
library(here)
library(plotly)
library(haven)
library(tools)

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

############# Plots

# EA confidence indicator
bcs_m_ea_con <- plot_ly(bcs_m, x = ~period, y = ~INDU.EA.10.COF.B.M, type = 'scatter', mode = 'lines')

bcs_m_ea_con <- bcs_m_ea_con %>%
  layout(title = '',
         xaxis = list(title = 'Period'),
         yaxis = list(title = 'Balance')) %>%
  config(displayModeBar = FALSE)

# Show the plot
bcs_m_ea_con

# EA production trend in recent months
bcs_m_ea_prod_trend <- plot_ly(bcs_m, x = ~period, y = ~INDU.EU.10.1.B.M, type = 'scatter', mode = 'lines') %>%
  layout(title = '',
         xaxis = list(title = 'Period'),
         yaxis = list(title = 'Balance')) %>%
  config(displayModeBar = FALSE)

