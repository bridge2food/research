library(tidyverse)
library(here)
library(plotly)
library(haven)

source("functions.R")

ppus <- read_sav(paste0(here(),"/data_raw/qualtrics/", "PPUS Q2 2024 - test.sav"))

q2.2_bar <- v_bar_chart(ppus, "Q2.2", "this is the title")

q2.2_sai <- simple_average_index(ppus, "Q2.2")

