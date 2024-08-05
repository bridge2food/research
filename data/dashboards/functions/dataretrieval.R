#################### Data retrieval functions

latest_data <- function(survey_name) {
  period <- latest_period(survey_name)
  get_data(survey_name, period$year, period$quarter)
}

prev_q_data <- function(survey_name) {
  period <- previous_period(survey_name)
  get_data(survey_name, period$year, period$quarter)
}

prev_y_data <- function(survey_name) {
  period <- latest_period(survey_name)
  prev_year <- period$year - 1
  get_data(survey_name, prev_year, period$quarter)
}

# Function to get data for a period specified in the format "YYYY-QN"
specified_period_data <- function(survey_name, period) {
  parts <- strsplit(period, "-Q")[[1]]
  year <- as.numeric(parts[1])
  quarter <- paste0("Q", parts[2])
  get_data(survey_name, year, quarter)
}

# Function to get data for a specified year and quarter
get_data <- function(survey_name, year, quarter) {
  files <- list_survey_files(survey_name)
  if (length(files) == 0) stop("No files found for the given survey name")
  time_info <- extract_time_info(files)
  specific_file <- time_info$file[time_info$year == year & time_info$quarter == quarter]
  if (length(specific_file) == 0) stop("No file found for the specified quarter and year")
  read_sav(specific_file)
}

# Function to get the latest period (year and quarter) for a given survey name
latest_period <- function(survey_name) {
  files <- list_survey_files(survey_name)
  if (length(files) == 0) stop("No files found for the given survey name")
  time_info <- extract_time_info(files)
  latest_year <- max(time_info$year, na.rm = TRUE)
  latest_quarter <- max(time_info$quarter[time_info$year == latest_year], na.rm = TRUE)
  list(year = latest_year, quarter = latest_quarter)
}

# Function to get the previous period (year and quarter) for a given survey name
previous_period <- function(survey_name) {
  period <- latest_period(survey_name)
  latest_year <- period$year
  latest_quarter <- period$quarter
  quarter_order <- c("Q1", "Q2", "Q3", "Q4")
  latest_quarter_index <- match(latest_quarter, quarter_order)
  if (latest_quarter_index == 1) {
    prev_year <- latest_year - 1
    prev_quarter <- "Q4"
  } else {
    prev_year <- latest_year
    prev_quarter <- quarter_order[latest_quarter_index - 1]
  }
  list(year = prev_year, quarter = prev_quarter)
}